{-# LANGUAGE TemplateHaskell #-}

module Data.ConfigGen.Parsing
    ( ParsedTypes(..)
    , createModule
    ) where

import Control.Lens               (makeLenses, (%~), (&), (.~), (<&>), (?~),
                                   (^.))
import Control.Monad              (liftM2, when)
import Control.Monad.State.Strict (StateT (StateT, runStateT), get, modify,
                                   withStateT)
import Data.Foldable              (Foldable (foldl'), foldlM)

import qualified Data.Aeson.Key    as K (fromString, toString)
import           Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import           Data.Aeson.Types  (prependFailure, typeMismatch)
import           Data.Yaml         (Array, FromJSON (..), Object, Parser,
                                    Value (..), withArray, withText)

import qualified Data.Bifunctor
import           Data.Maybe     (catMaybes, fromJust, fromMaybe, isJust,
                                 isNothing)
import           Data.Set       (Set)
import qualified Data.Set       as Set
import           Data.String    (IsString (fromString))
import qualified Data.Text      as T

import GHC.IO        (unsafePerformIO)
import GHC.SourceGen (HsDecl', HsModule', HsType', data', field, import',
                      module', qualified', recordCon, strict, var)

import Control.Monad.Reader (MonadReader (ask), Reader)
import Data.Char            (toLower, toUpper)
import Debug.Trace          (trace)
import System.FilePath      (replaceExtension, splitDirectories)
import System.Random        (randomIO)

newtype Dependency =
    Dependency String
    deriving newtype (Show, Eq, Ord)

data DependencyTracker =
    DependencyTracker
        { _dependancySet :: Set Dependency
        , _lastAdded     :: Maybe Dependency
        }

newtype NameSpace =
    NameSpace [String]
    deriving newtype (Show, Eq, Ord, Semigroup, Monoid)

appendToNameSpace :: String -> NameSpace -> NameSpace
appendToNameSpace s (NameSpace ss) = NameSpace $ s : ss

getNameSpaceHead :: NameSpace -> Maybe String
getNameSpaceHead (NameSpace [])    = Nothing
getNameSpaceHead (NameSpace (s:_)) = Just s

type DeclMap = KeyMap ModuleParts

data ModuleParts =
    ModuleParts
        { _declaration          :: Maybe HsDecl'
        , _structureType        :: HsType'
        , _externalDependencies :: DependencyTracker
        , _localDependencies    :: DeclMap
        , _isLocal              :: Bool
        }

data ParserState =
    ParserState
        { _cachedIncludes :: DeclMap
        , _nameSpace      :: NameSpace
        }

initialParserState :: ParserState
initialParserState = ParserState mempty mempty

-- declmap: path -> (decl?, type)
type StatefulParse = StateT ParserState Parser

-- state : declmap
-- return: Parser (decl?, type)
data ParsedTypes =
    ParsedTypes
        { _parsedModule :: ModuleParts
        , _parserState  :: ParserState
        }

makeLenses ''DependencyTracker

makeLenses ''ModuleParts

makeLenses ''ParserState

makeLenses ''ParsedTypes

removeSelf :: DependencyTracker -> DependencyTracker
removeSelf dt =
    dt & dependancySet %~ maybe id Set.delete (dt ^. lastAdded) & lastAdded .~ Nothing

addSelf :: Dependency -> DependencyTracker -> DependencyTracker
addSelf d dt = dt & dependancySet %~ Set.insert d & lastAdded ?~ d

emptyDep :: DependencyTracker
emptyDep = DependencyTracker mempty Nothing

fmergeDeps ::
       (DependencyTracker -> DependencyTracker -> Maybe Dependency)
    -> DependencyTracker
    -> DependencyTracker
    -> DependencyTracker
fmergeDeps f d2 d1 = DependencyTracker (d1 ^. dependancySet <> d2 ^. dependancySet) $ f d1 d2

mergeDeps :: DependencyTracker -> DependencyTracker -> DependencyTracker
mergeDeps = fmergeDeps (\_ _ -> Nothing)

instance FromJSON ParsedTypes where
    parseJSON (Object km) =
        (uncurry ParsedTypes <$> runStateT (parseDispatch km Nothing) initialParserState) <&>
        (parsedModule . externalDependencies %~ removeSelf)
    parseJSON invalid =
        prependFailure
            "parsing of JSON-scheme failed, probably encoured something wrong or somewhere wrong" $
        typeMismatch "Parsed Types" invalid

makeStateful :: Functor m => m a -> StateT s m a
makeStateful x = StateT $ \s -> (, s) <$> x

expectObject :: forall a. String -> (Object -> StatefulParse a) -> Value -> StatefulParse a
expectObject _ f (Object o) = f o
expectObject msg _ v = makeStateful $ prependFailure msg (typeMismatch "Object" v)

expectString :: forall a. String -> (T.Text -> StatefulParse a) -> Value -> StatefulParse a
expectString _ f (String o) = f o
expectString msg _ v = makeStateful $ prependFailure msg (typeMismatch "String" v)

expectArray :: forall a. String -> (Array -> StatefulParse a) -> Value -> StatefulParse a
expectArray _ f (Array o) = f o
expectArray msg _ v = makeStateful $ prependFailure msg (typeMismatch "Array" v)

-- todo: the rest
-- Bool, Null and something else are missing but they are not used anywhere yet
parseDispatch :: Object -> Maybe String -> StatefulParse ModuleParts
parseDispatch obj name
    | Nothing <- KM.lookup "type" obj = failure "type field not found" $ Object obj
    | Just jsType <- KM.lookup "type" obj
    , jsType /= String "object" =
        makeStateful $ parsePrimitiveType jsType (KM.lookup "haskell/type_info" obj)
    | Just origin <- KM.lookup "haskell/origin" obj = do
        oldNameSpace <- get <&> _nameSpace
        -- running on include with fresh namespace
        res <- withStateT (nameSpace .~ mempty) (parseInclude obj name origin)
        modify $ nameSpace .~ oldNameSpace
        return res
    | otherwise = parseAnonymous obj name

parseInclude :: Object -> Maybe String -> Value -> StatefulParse ModuleParts
parseInclude obj name origin = do
    uriKey <-
        K.fromString . T.unpack <$>
        expectString "parsing \"haskell/origin\" field" return origin
    maybeTypePair <- KM.lookup uriKey . _cachedIncludes <$> get
    (externalDependencies %~ addSelf (Dependency . K.toString $ uriKey)) <$>
        case (maybeTypePair :: Maybe ModuleParts) of
            Just typePair -> return typePair
            Nothing -> do
                title <- makeStateful $ getTitleFromObject obj name
                modify $ nameSpace %~ appendToNameSpace title
                newTypePair <- parseRecordType obj <&> isLocal .~ False
                modify $ cachedIncludes %~ KM.insert uriKey newTypePair
                return newTypePair

parseAnonymous :: Object -> Maybe String -> StatefulParse ModuleParts
parseAnonymous obj name = do
    title <- makeStateful $ getTitleFromObject obj name
    oldNameSpace <- get <&> _nameSpace
    me <- withStateT (nameSpace %~ appendToNameSpace title) (parseRecordType obj)
    modify $ nameSpace .~ oldNameSpace
    return me

getTitleFromObject :: Object -> Maybe String -> Parser String
getTitleFromObject obj name = do
    title <-
        fmap T.unpack <$>
        sequence
            (withText "parsing of title failed. Should be text " return <$>
             KM.lookup "title" obj)
    case (title, name) of
        (Nothing, Nothing) ->
            fail $
            "Either there is need to be a title or an external name. There are non here: " ++
            show obj
        (Just t, Nothing) -> return t
        (Nothing, Just n) -> return n
        (Just t, Just _) -> return t

parsePrimitiveType :: Value -> Maybe Value -> Parser ModuleParts
parsePrimitiveType (String jsType) Nothing =
    (\t -> ModuleParts Nothing t emptyDep mempty False) <$>
    case jsType of
        "scientific" -> return . var $ "GHC.Types.Int"
        "number"     -> return . var $ "GHC.Types.Int"
        "text"       -> return . var $ "Data.Text.Text"
        "string"     -> return . var $ "Data.Text.Text"
        invalid      -> fail $ "illigal string in type: " ++ T.unpack invalid
parsePrimitiveType (String _) (Just (String typeInfo)) =
    (\t -> ModuleParts Nothing t emptyDep mempty False) <$>
    case typeInfo of
        "Int64" -> return . var $ "GHC.Int.Int64"
        invalid -> fail $ "invalid haskell/type_info" ++ T.unpack invalid
parsePrimitiveType _ _ = fail "jsType and haskell/type_info should both be strings"

parseRecordType :: Object -> StatefulParse ModuleParts
parseRecordType obj = do
    reqs <-
        makeStateful $
        maybe
            (return mempty)
            (withArray "parsing \"required\" field" parseReqs)
            (KM.lookup "required" obj)
    properties <-
        maybe
            (emptyProps reqs)
            (expectObject
                 "parsing of JSON-scheme failed, properties must be Object"
                 (parseProps reqs))
            (KM.lookup "properties" obj)
    let fields = KM.toAscList $ strict . field . _structureType <$> properties
    maybeName <- get <&> getNameSpaceHead . _nameSpace
    when (isNothing maybeName) $
        fail ("while parsing record type encounterd an empty namespace. " ++ show obj)
    let name = capitalizeFirstLetter . fromJust $ maybeName
    let recordCntr =
            recordCon (fromString name) $ Data.Bifunctor.first (fromString . K.toString) <$>
            fields
    let extDeps =
            foldl' (\acc d -> mergeDeps (d ^. externalDependencies) acc) emptyDep properties
    let localDeps = KM.filter _isLocal properties
    return $
        ModuleParts
            (Just $ data' (fromString name) [] [recordCntr] [])
            (var $ fromString name)
            extDeps
            localDeps
            True
  where
    emptyProps :: [String] -> StatefulParse (KeyMap ModuleParts)
    emptyProps reqs'
        | null reqs' = return mempty
        | otherwise = fail "properties are emtpy, but requirements are not"
    parseReqs :: Array -> Parser [String]
    parseReqs =
        foldlM
            (\acc x ->
                 liftM2
                     (:)
                     (withText
                          "parsing one of the elements of \"required\" field"
                          (return . T.unpack)
                          x)
                     (return acc))
            mempty

parseProps :: [String] -> Object -> StatefulParse (KeyMap ModuleParts)
parseProps reqs props
    | all (\k -> K.fromString k `KM.member` props) reqs =
        KM.traverseWithKey
            (\k v ->
                 expectObject
                     "parsing of JSON-scheme failed, each property must be an Object"
                     (`parseDispatch` (Just . K.toString $ k))
                     v)
            props
    | otherwise =
        failure
            "parsing of JSON-scheme failed, required list does not match content"
            (Object props)

failure :: String -> Value -> StatefulParse a
failure msg v = makeStateful $ prependFailure msg (typeMismatch "Parsed Types" v)

createModule :: ParsedTypes -> HsModule'
createModule pt =
    let locDeclMap = extractLocalDependencies (pt ^. parsedModule)
        locDeclMapExt =
            KM.foldl'
                (\acc it -> acc <> extractLocalDependencies it)
                mempty
                (pt ^. parserState . cachedIncludes)
        locDecl = (snd <$> KM.toList locDeclMapExt) <> (snd <$> KM.toList locDeclMap)
        extDeclMap = pt ^. parserState . cachedIncludes <&> _declaration
        extDecls =
            catMaybes $ (pt ^. parsedModule . declaration) : (snd <$> KM.toAscList extDeclMap)
     in module' (Just "Generated") exports imports (locDecl ++ extDecls)
  where
    imports = qualified' <$> [import' "GHC.Types", import' "GHC.Int", import' "Data.Text"]
    exports = Nothing -- export everything?

extractLocalDependencies :: ModuleParts -> KeyMap HsDecl'
extractLocalDependencies mp =
    let locDeps = mp ^. localDependencies
        baseCase =
            fromJust . _declaration <$> KM.filter (\mp' -> isJust $ mp' ^. declaration) locDeps
        recursion = KM.foldl' (\acc v -> acc <> extractLocalDependencies v) mempty locDeps
     in baseCase <> recursion

capitalizeFirstLetter :: String -> String
capitalizeFirstLetter []     = []
capitalizeFirstLetter (x:xs) = toUpper x : xs
-- createModules :: ModuleParts -> Reader ParserState (KeyMap HsModule')
-- createModules mp = do
--     depsPool <- ask <&> _cachedIncludes
--     let deps = mp ^. externalDependencies . dependancySet
--     return undefined
--   where
--     makeModuleNameOutOfPath :: FilePath -> String
--     makeModuleNameOutOfPath p =
--         let capitalizeFirstLetter x = toUpper (head x) : (toLower <$> tail x)
--          in mconcat $ capitalizeFirstLetter <$> (splitDirectories . replaceExtension ".hs" $ p)
{-
----------------------------------
latex-request-object-inc.yaml

type: object
title: LatexRequest
required:
  - ratio
properties:
  ratio: !include "./ratio.yaml"
  glossary:
    type: object
    properties:
      size:
        type: number
      name:
        type: text
      somthingelse:
        type: number

----------------------------------
ratio.yaml

type: object
properties:
  num:
    type: number
    haskell/type_info: "Int64"
  denum:
    type: number

----------------------------------
Generated.hs

module Generated where
import qualified GHC.Types
import qualified GHC.Int
import qualified Data.Text
data Glossary
  = Glossary {name :: !Data.Text.Text,
              size :: !GHC.Types.Int,
              somthingelse :: !GHC.Types.Int}
data LatexRequest
  = LatexRequest {glossary :: !Glossary, ratio :: !Ratio}
data Ratio = Ratio {denum :: !GHC.Types.Int, num :: !GHC.Int.Int64}

-}
