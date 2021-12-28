{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.ConfigGen.Parsing where

import Control.Lens               (makeLenses, (%~), (&), (.~), (<&>), (^.))
import Control.Monad.State.Strict (MonadTrans (lift), StateT (runStateT), get, modify)

import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Yaml        (Array, FromJSON (..), Object, Parser, ToJSON, Value (..), (.!=), (.:),
                         (.:?))

import qualified Data.Bifunctor
import           Data.Maybe     (fromJust, fromMaybe)
import qualified Data.Set       as Set
import           Data.String    (IsString (fromString))

import           Control.Applicative           ((<|>))
import qualified Data.ConfigGen.JSTypes        as JS
import           Data.ConfigGen.ModuleParts    (ModuleParts (..))
import qualified Data.ConfigGen.ModuleParts    as MP
import qualified Data.ConfigGen.Parsing.LCP    as LCP
import           Data.ConfigGen.Traverse.Utils (Title)
import qualified Data.ConfigGen.Traverse.Utils as U
import qualified Data.ConfigGen.TypeRep        as TR
import           Data.List                     (intersperse, stripPrefix, (\\))
import           Data.List.Utils               (split)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           GHC.Generics                  (Generic)
import           System.FilePath               (pathSeparator, takeBaseName, (</>))

newtype ParserState =
    ParserState
        { _cachedIncludes :: Map FilePath ModuleParts
        }
    deriving (Show, Eq, Generic)
    deriving newtype (Semigroup, Monoid)
    deriving anyclass (ToJSON)

type StatefulParser a = StateT ParserState Parser a

data ParserResult =
    ParserResult
        { mainType :: ModuleParts
        , deps     :: [(TR.ModuleName, ModuleParts)]
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON)

makeLenses ''ModuleParts

makeLenses ''ParserResult

makeLenses ''ParserState

type TypeInfo = String

type Origin = FilePath

type Properties = Object

instance FromJSON ParserResult where
    parseJSON (Object obj) = makeParserResult <$> runStateT (parseDispatch obj) mempty
      where
        makeParserResult :: (ModuleParts, ParserState) -> ParserResult
        makeParserResult (mainType, ps) =
            let deps = Map.toList (_cachedIncludes ps)
             in ParserResult {..}
    parseJSON invalid = prependFailure failMsg $ typeMismatch "Parsed Types" invalid
      where
        failMsg = "parsing of JSON-scheme failed, expected object encountered something else. "

parseDispatch :: Object -> StatefulParser ModuleParts
parseDispatch obj = do
    jsTypeTag <- lift failOrJsTypeTag
    maybeOrigin <- lift $ obj .:? "haskell/origin"
    typeInfo <- lift $ obj .:? "haskell/type-info"
    title <- lift $ obj .:? "title"
    m_enum <- lift $ obj .:? "enum"
    retrieveCachedInclude title maybeOrigin $
        parseOneOf obj <|> parseAnyOf obj <|> parseAllOf obj <|>
        case jsTypeTag of
            JS.Prim tag ->
                case (tag, m_enum) of
                    (JS.PrimStringTag, Just (enum :: [String])) ->
                        return $ parseStringEnum enum title
                    (tag', _) -> parsePrimitve tag' title typeInfo
            JS.Rec tag -> parseRecordLike obj tag title
            JS.ArrayTag -> parseArray obj title
  where
    failOrJsTypeTag :: Parser JS.TypeTag
    failOrJsTypeTag = do
        (maybeJsTypeTag :: Maybe JS.TypeTag) <-
            JS.parseTypeTag <$> (obj .:? "type" .!= "object")
        maybe (fail $ "Found incorect type here: " ++ show obj) return maybeJsTypeTag
    retrieveCachedInclude ::
           Maybe Title
        -> Maybe Origin
        -> StatefulParser ModuleParts -- how to cache if not yet cached
        -> StatefulParser ModuleParts
    retrieveCachedInclude title m_origin k =
        (\t -> maybe k t m_origin) $ \origin -> do
            maybeCachedIncldue <- fmap (Map.lookup origin) $ get <&> _cachedIncludes
            case maybeCachedIncldue of
                Nothing -> do
                    res <- k
                    modify $ cachedIncludes %~ Map.insert origin res
                Just _ -> return ()
            return . ModuleParts title mempty mempty . TR.Ref $
                TR.RefExternalType origin (U.typeNameFromAbsolutePath origin title)

parseOneOf :: Object -> StatefulParser ModuleParts
parseOneOf obj = do
    (options :: Map String (ModuleParts, Bool)) <-
        Map.map (, True) . Map.fromList . zip (fmap (\n -> "Unnamed" ++ show n) [1 :: Int ..]) <$>
        (traverse parseDispatch =<< lift (obj .: "oneOf"))
    title <- lift $ obj .:? "title"
    let ini = ModuleParts title mempty mempty $ TR.SumType mempty
    return $ Map.foldrWithKey MP.appendRecord ini options

parseAnyOf :: Object -> StatefulParser ModuleParts
parseAnyOf obj = do
    (_ :: Array) <- lift (obj .: "anyOf")
    title <- lift $ obj .:? "title"
    return $ ModuleParts title mempty mempty TR.AnyOf

parseAllOf :: Object -> StatefulParser ModuleParts
parseAllOf obj = do
    (_ :: Array) <- lift (obj .: "allOf")
    title <- lift $ obj .:? "title"
    return $ ModuleParts title mempty mempty TR.AllOf

parseArray :: Object -> Maybe Title -> StatefulParser ModuleParts
parseArray obj maybeTitle = do
    let itemField :: String = "items"
    itemsModule <- parseDispatch =<< lift (obj .: fromString itemField)
    case itemsModule ^. declaration of
        (TR.Ref (TR.RefExternalType s tn)) ->
            return $
            ModuleParts maybeTitle (Set.singleton s) mempty $
            TR.ArrayType (TR.ReferenceToExternalType s tn)
        (TR.Ref (TR.RefPrimitiveType s)) ->
            return $
            ModuleParts maybeTitle mempty mempty $ TR.ArrayType (TR.ReferenceToPrimitiveType s)
        _local ->
            let tn = U.chooseName itemField (itemsModule ^. jsTitle)
             in return $
                ModuleParts maybeTitle mempty (Map.singleton itemField itemsModule) $
                TR.ArrayType (TR.ReferenceToLocalType itemField tn)

parseStringEnum :: [String] -> Maybe Title -> ModuleParts
parseStringEnum enum title = ModuleParts title mempty mempty typeRep
  where
    typeRep = TR.SumType . Map.fromList $ (, TR.SumConstr []) <$> enum

parsePrimitve :: JS.PrimitiveTag -> Maybe Title -> Maybe TypeInfo -> StatefulParser ModuleParts
parsePrimitve typeTag maybeTitle tInfo = do
    let newtypeWrapper =
            case maybeTitle of
                Just _  -> TR.NewType . TR.ExtRef
                Nothing -> TR.Ref
    return . ModuleParts maybeTitle mempty mempty . newtypeWrapper . TR.RefPrimitiveType $
        case typeTag of
            JS.PrimStringTag -> fromMaybe "Data.Text.Text" tInfo
            JS.PrimIntTag    -> fromMaybe "Int" tInfo
            JS.PrimDoubleTag -> fromMaybe "Data.Scientific.Scientific" tInfo
            JS.PrimBoolTag   -> "Bool"
            JS.PrimNullTag   -> "()"

parseRecordLike :: Object -> JS.RecordLikeTag -> Maybe Title -> StatefulParser ModuleParts
parseRecordLike obj typeTag title = do
    (reqs :: [TR.FieldName]) <- lift $ obj .:? "required" .!= mempty
    (properties :: Map TR.FieldName (Object, Bool)) <-
        lift $ checkRequired reqs =<< obj .:? "properties" .!= mempty
    (parsedProperties :: Map TR.FieldName (ModuleParts, Bool)) <-
        mapM
            (\(obj', req) -> do
                 mp <- parseDispatch obj'
                 return (mp, req))
            properties
    let initialTypeRep =
            case typeTag of
                JS.RecEnumTag   -> TR.SumType mempty
                JS.RecObjectTag -> TR.ProdType mempty
    return $
        Map.foldrWithKey
            MP.appendRecord
            (ModuleParts title mempty mempty initialTypeRep)
            parsedProperties
  where
    checkRequired ::
           [TR.FieldName]
        -> Map TR.FieldName Object
        -> Parser (Map TR.FieldName (Object, Bool))
    checkRequired reqs properties =
        if all (`Map.member` properties) reqs
            then do
                let setReqs = Set.fromList reqs
                return $ Map.mapWithKey (\k v -> (v, k `Set.member` setReqs)) properties
            else fail $
                 "There are some fields in " ++
                 fromMaybe "Unnamed" title ++
                 " that are required but not present: " ++
                 (mconcat . intersperse ", " $ reqs \\ Map.keys properties)

postprocessParserResult :: ParserResult -> ParserResult
postprocessParserResult (ParserResult mp incs) =
    ParserResult (go mp) $ Data.Bifunctor.bimap (fromJust . changePath) go <$> incs
  where
    changePath y =
        case incs of
            [] -> Just $ addPrefix . takeBaseName $ y
            [_] -> Just $ addPrefix . takeBaseName $ y
            _ ->
                let lcp = LCP.commonPrefix $ split [pathSeparator] . fst <$> incs
                 in addPrefix . mconcat . intersperse [pathSeparator] <$>
                    stripPrefix lcp (split [pathSeparator] y)
      where
        addPrefix :: FilePath -> FilePath
        addPrefix p = U.globalPrefix </> p
    go :: ModuleParts -> ModuleParts
    go mp'
        | TR.ProdType km <- tr = mpu' & declaration .~ TR.ProdType (mapField <$> km)
        | TR.SumType km <- tr = mpu' & declaration .~ TR.SumType (fmap mapField <$> km)
        | TR.ArrayType tr' <- tr = mpu' & declaration .~ TR.ArrayType (mapTypeRef tr')
        | TR.NewType tr' <- tr = mpu' & declaration .~ TR.NewType (mapTypeRef tr')
        | TR.Ref nltr <- tr = mpu' & declaration .~ TR.Ref (mapNonLocalRef nltr)
        | otherwise = mp'
      where
        mapField = \(TR.Field req tr') -> TR.Field req $ mapTypeRef tr'
        tr = _declaration mp'
        mpu = mp' & externalDeps %~ Set.map (fromJust . changePath)
        mpu' = mpu & localDeps %~ fmap go
    mapNonLocalRef :: TR.NonLocalRef -> TR.NonLocalRef
    mapNonLocalRef tr
        | TR.RefExternalType s tn <- tr = TR.RefExternalType (fromJust $ changePath s) tn
        | otherwise = tr
    mapTypeRef :: TR.TypeRef -> TR.TypeRef
    mapTypeRef (TR.ExtRef nlr) = TR.ExtRef $ mapNonLocalRef nlr
    mapTypeRef r               = r

dashesToUnderscore :: String -> String
dashesToUnderscore = map l
  where
    l '-' = '_'
    l s   = s

transformStrings :: (String -> String) -> ParserResult -> ParserResult
transformStrings transform (ParserResult mp deps) =
    ParserResult (go mp) $ Data.Bifunctor.bimap transform go <$> deps
  where
    go :: ModuleParts -> ModuleParts
    go (ModuleParts m_s set km tr) = ModuleParts {..}
      where
        _jsTitle = transform <$> m_s
        _localDeps = Map.mapKeys transform . Map.map go $ km
        _externalDeps = Set.map transform set
        _declaration = transformTypeRep tr
    transformTypeRep :: TR.TypeRep -> TR.TypeRep
    transformTypeRep (TR.ProdType km) =
        TR.ProdType $ Map.mapKeys transform . Map.map transformField $ km
    transformTypeRep (TR.SumType km) =
        TR.SumType $ Map.mapKeys transform . Map.map (fmap transformField) $ km
    transformTypeRep (TR.ArrayType tr') = TR.ArrayType $ transfromTypeRef tr'
    transformTypeRep (TR.NewType tr') = TR.NewType $ transfromTypeRef tr'
    transformTypeRep (TR.Ref (TR.RefExternalType nm tn)) =
        TR.Ref $ TR.RefExternalType (transform nm) (transform tn)
    transformTypeRep (TR.Ref (TR.RefPrimitiveType nm)) = TR.Ref . TR.RefPrimitiveType $ nm
    transformTypeRep x = x
    transfromTypeRef :: TR.TypeRef -> TR.TypeRef
    transfromTypeRef (TR.ReferenceToLocalType s tn) =
        TR.ReferenceToLocalType (transform s) (transform tn)
    transfromTypeRef (TR.ReferenceToExternalType s tn) =
        TR.ReferenceToExternalType (transform s) (transform tn)
    transfromTypeRef r = r
    transformField :: TR.Field -> TR.Field
    transformField (TR.Field req tr') = TR.Field req $ transfromTypeRef tr'
