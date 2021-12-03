{-# LANGUAGE TemplateHaskell #-}

module Data.ConfigGen.Parsing
    ( ParsedTypes(..)
    , createModule
    ) where

import Control.Lens               (makeLenses, (%~), (&), (.~), (<&>), (?~),
                                   (^.))
import Control.Monad              (liftM2)
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
import           Data.Maybe     (catMaybes)
import           Data.Set       (Set)
import qualified Data.Set       as Set
import           Data.String    (IsString (fromString))
import qualified Data.Text      as T

import GHC.IO        (unsafePerformIO)
import GHC.SourceGen (HsDecl', HsModule', HsType', data', field, import',
                      module', qualified', recordCon, strict, var)

import Control.Monad.Reader (MonadReader (ask), Reader)
import Data.Char            (toLower, toUpper)
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

type DeclMap = KeyMap ModuleParts

data ModuleParts =
    ModuleParts
        { _declaration          :: Maybe HsDecl'
        , _structureType        :: HsType'
        , _externalDependencies :: DependencyTracker
        , _localDependencies    :: DeclMap
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
        (uncurry ParsedTypes <$> runStateT (parseDispatch km) initialParserState) <&>
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
parseDispatch :: Object -> StatefulParse ModuleParts
parseDispatch obj
    | Nothing <- KM.lookup "type" obj = failure "type field not found" $ Object obj
    | Just jsType <- KM.lookup "type" obj
    , jsType /= String "object" =
        makeStateful $ parsePrimitiveType jsType (KM.lookup "haskell/type_info" obj)
    | Just origin <- KM.lookup "haskell/origin" obj = do
        oldNameSpace <- get <&> _nameSpace
        -- running on include with fresh namespace
        withStateT (nameSpace .~ oldNameSpace) $
            withStateT (nameSpace .~ mempty) (parseInclude origin)
    | otherwise = do
        title <- getTitleFromObject
        parseRecordType obj title
  where
    parseInclude :: Value -> StatefulParse ModuleParts
    parseInclude origin = do
        uriKey <-
            K.fromString . T.unpack <$>
            expectString "parsing \"haskell/origin\" field" return origin
        maybeTypePair <- KM.lookup uriKey . _cachedIncludes <$> get
        (externalDependencies %~ addSelf (Dependency . K.toString $ uriKey)) <$>
            case (maybeTypePair :: Maybe ModuleParts) of
                Just typePair -> return typePair
                Nothing -> do
                    newTypePair <- parseRecordType obj =<< getTitleFromObject
                    modify $ cachedIncludes %~ KM.insert uriKey newTypePair
                    return newTypePair
    getTitleFromObject :: StatefulParse String
    getTitleFromObject = do
        titleSource <-
            makeStateful $
            sequence
                (withText "parsing of title failed. Should be text " return <$>
                 KM.lookup "title" obj)
        return $
            case titleSource of
                Nothing  -> "UntitledType_" ++ (show . abs $ unsafeName)
                Just txt -> T.unpack txt
      where
        {-# NOINLINE unsafeName #-}
        unsafeName = unsafePerformIO (randomIO :: IO Int)

parsePrimitiveType :: Value -> Maybe Value -> Parser ModuleParts
parsePrimitiveType (String jsType) Nothing =
    (\t -> ModuleParts Nothing t emptyDep mempty) <$>
    case jsType of
        "scientific" -> return . var $ "GHC.Types.Int"
        "number"     -> return . var $ "GHC.Types.Int"
        "text"       -> return . var $ "Data.Text.Text"
        "string"     -> return . var $ "Data.Text.Text"
        invalid      -> fail $ "illigal string in type: " ++ T.unpack invalid
parsePrimitiveType (String _) (Just (String typeInfo)) =
    (\t -> ModuleParts Nothing t emptyDep mempty) <$>
    case typeInfo of
        "Int64" -> return . var $ "Data.Int.Int64"
        invalid -> fail $ "invalid haskell/type_info" ++ T.unpack invalid
parsePrimitiveType _ _ = fail "jsType and haskell/type_info should both be strings"

parseRecordType :: Object -> String -> StatefulParse ModuleParts
parseRecordType obj name = do
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
    let recordCntr =
            recordCon (fromString name) $ Data.Bifunctor.first (fromString . K.toString) <$>
            fields
    let extDeps =
            foldl' (\acc d -> mergeDeps (d ^. externalDependencies) acc) emptyDep properties
    return $
        ModuleParts
            (Just $ data' (fromString name) [] [recordCntr] [])
            (var $ fromString name)
            extDeps
            mempty
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
        traverse
            (expectObject
                 "parsing of JSON-scheme failed, each property must be an Object"
                 parseDispatch)
            props
    | otherwise =
        failure
            "parsing of JSON-scheme failed, required list does not match content"
            (Object props)

failure :: String -> Value -> StatefulParse a
failure msg v = makeStateful $ prependFailure msg (typeMismatch "Parsed Types" v)

createModule :: ParsedTypes -> HsModule'
createModule pt =
    let declMap = pt ^. parserState . cachedIncludes <&> _declaration
        decls = catMaybes $ (pt ^. parsedModule . declaration) : (snd <$> KM.toAscList declMap)
     in module' (Just "Generated") exports imports decls
  where
    imports = qualified' <$> [import' "GHC.Types", import' "GHC.Int", import' "Data.Text"]
    exports = Nothing -- export everything?

createModules :: ModuleParts -> Reader ParserState (KeyMap HsModule')
createModules mp = do
    depsPool <- ask <&> _cachedIncludes
    let deps = mp ^. externalDependencies . dependancySet
    return undefined
  where
    makeModuleNameOutOfPath :: FilePath -> String
    makeModuleNameOutOfPath p =
        let capitalizeFirstLetter x = toUpper (head x) : (toLower <$> tail x)
         in mconcat $ capitalizeFirstLetter <$> (splitDirectories . replaceExtension ".hs" $ p)
{-
----------------------------------
latex-request-object-inc.yaml

type: object
title: LatexRequest
required:
  - ratio
properties:
  ratio: !include "./ratio.yaml"

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
data LatexRequest
  = LatexRequest {ratio :: !UntitledType_3670921995040036773}
data UntitledType_3670921995040036773
  = UntitledType_3670921995040036773 {denum :: !GHC.Types.Int,
                                      num :: !Data.Int.Int64}}

-}
