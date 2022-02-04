{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.TransportTypes.Parsing where

import Control.Lens (makeLenses, (%~), (&), (.~), (<&>), (^.))

import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Yaml        (FromJSON (..), Object, Parser, ToJSON, Value (..), encode, object,
                         (.!=), (.:), (.:?), (.=))

import           Control.Monad.State.Strict (MonadTrans (lift), StateT (runStateT), get,
                                             modify)
import qualified Data.Bifunctor
import           Data.List                  (intersperse, stripPrefix, (\\))
import           Data.List.Split            (splitOn)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromJust, fromMaybe)
import qualified Data.Set                   as Set
import           Data.String                (IsString (fromString))
import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8)
import           GHC.Generics               (Generic)
import           System.FilePath            (pathSeparator, takeBaseName, (</>))

import           Data.Foldable                           (asum)
import qualified Data.Text                               as T
import qualified Data.TransportTypes.CodeGen.NamingUtils as U
import qualified Data.TransportTypes.JSTypes             as JS
import           Data.TransportTypes.ModuleParts         (ModuleParts (..))
import qualified Data.TransportTypes.ModuleParts         as MP
import qualified Data.TransportTypes.Parsing.LCP         as LCP
import qualified Data.TransportTypes.TypeRep             as TR

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

type EncodedJSON = Text

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
    maybeOrigin <- lift $ obj .:? "haskell/origin"
    title <- lift $ obj .:? "title"
    retrieveCachedInclude title maybeOrigin . asum $
        [ parseOneOf
        , parseAnyOf
        , parseAllOf
        , parseConst
        , parseArray
        , parseRecordLike
        , parseEnum
        , parsePrim
        , parseUnknon
        ] <*>
        pure obj
  where
    retrieveCachedInclude ::
           Maybe U.Title
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
            return $
                ModuleParts
                    title
                    mempty
                    mempty
                    (TR.Ref $
                     TR.RefExternalType origin (U.typeNameFromAbsolutePath origin title))
                    mempty

checkType :: Object -> (Maybe JS.TypeTag -> StatefulParser a) -> StatefulParser a
checkType obj k = k . JS.parseTypeTag =<< lift (obj .: "type")

parseUnknon :: Object -> StatefulParser ModuleParts
parseUnknon obj -- fail $ "No option for parsing:\n" ++ (T.unpack . decodeUtf8 . encode $  obj)
 = do
    (typetag :: Maybe String) <- lift $ obj .:? "type"
    case typetag of
        Nothing -> do
            let enc = decodeUtf8 . encode $ obj
            title <- lift $ obj .:? "title"
            return $
                ModuleParts
                    title
                    mempty
                    mempty
                    (TR.Ref $ TR.RefPrimitiveType "Data.Yaml.Object")
                    enc
        _ -> fail $ "This is some strange object that I am failed to parse:\n" ++ show obj

parsePrim :: Object -> StatefulParser ModuleParts
parsePrim obj = do
    tag <-
        checkType obj $ \case
            Just (JS.Prim tag) -> return tag
            _                  -> fail "Can't parse this as a primitive"
    title <- lift $ obj .:? "title"
    typeInfo <- lift $ obj .:? "haskell/type-info"
    let enc = decodeUtf8 . encode $ obj
    return $ parsePrimitve enc tag title typeInfo
  where
    parsePrimitve :: Text -> JS.PrimitiveTag -> Maybe U.Title -> Maybe TypeInfo -> ModuleParts
    parsePrimitve enc typeTag maybeTitle tInfo = ModuleParts maybeTitle mempty mempty rep enc
      where
        newtypeWrapper =
            case maybeTitle of
                Just _  -> TR.NewType . TR.ExtRef
                Nothing -> TR.Ref
        rep =
            newtypeWrapper . TR.RefPrimitiveType $
            case typeTag of
                JS.PrimStringTag -> fromMaybe "Data.Text.Text" tInfo
                JS.PrimIntTag    -> fromMaybe "Prelude.Int" tInfo
                JS.PrimDoubleTag -> fromMaybe "Data.Scientific.Scientific" tInfo
                JS.PrimBoolTag   -> "Prelude.Bool"
                JS.PrimNullTag   -> "Data.TransportTypes.Utils.UnitProxy"

parseEnum :: Object -> StatefulParser ModuleParts
parseEnum obj = do
    jsType :: Maybe String <- lift $ obj .:? "type"
    case jsType of
        Nothing -> return () -- because it is usually string and nobody writes type there
        _ ->
            checkType obj $ \case
                Just JS.Primitive -> return ()
                _                 -> fail "Can't parse this as an enum"
    enum <- lift $ obj .: "enum"
    title <- lift $ obj .:? "title"
    let enc = decodeUtf8 . encode $ obj
    return $ parseStringEnum enc enum title
  where
    parseStringEnum :: Text -> [String] -> Maybe U.Title -> ModuleParts
    parseStringEnum enc enum title = ModuleParts title mempty mempty typeRep enc
      where
        typeRep = TR.SumType $ (, TR.SumConstr []) <$> enum

parseConst :: Object -> StatefulParser ModuleParts
parseConst obj = do
    (cnst :: Value) <- lift (obj .: "const")
    title <- lift $ obj .:? "title"
    return $
        ModuleParts
            (decideCnstTitle title cnst)
            mempty
            mempty
            (TR.ConstType cnst)
            (decodeUtf8 . encode $ obj)
  where
    decideCnstTitle :: Maybe U.Title -> Value -> Maybe U.Title
    decideCnstTitle title cnst =
        case (cnst, title) of
            (String txt, Nothing) -> Just $ T.unpack txt
            _other                -> title

parseOneOf :: Object -> StatefulParser ModuleParts
parseOneOf obj = do
    (options :: Map String (ModuleParts, Bool)) <-
        Map.map (, True) . Map.fromList . zip (fmap (\n -> "Option" ++ show n) [1 :: Int ..]) <$>
        (traverse parseDispatch =<< lift (obj .: "oneOf"))
    title <- lift $ obj .:? "title"
    let ini = ModuleParts title mempty mempty (TR.OneOfType mempty) (decodeUtf8 . encode $ obj)
    return $ Map.foldrWithKey MP.appendRecord ini options

parseAnyOf :: Object -> StatefulParser ModuleParts
parseAnyOf obj = do
    (options :: [ModuleParts]) <- traverse parseDispatch =<< lift (obj .: "anyOf")
    title <- lift $ obj .:? "title"
    let ini = ModuleParts title mempty mempty (TR.AnyOfType mempty) (decodeUtf8 . encode $ obj)
    return $ foldr MP.appendAofPart ini $ zip [1 ..] options

parseAllOf :: Object -> StatefulParser ModuleParts
parseAllOf obj = do
    (options :: [ModuleParts]) <- traverse parseDispatch =<< lift (obj .: "allOf")
    title <- lift $ obj .:? "title"
    let ini = ModuleParts title mempty mempty (TR.AllOfType mempty) (decodeUtf8 . encode $ obj)
    return $ foldr MP.appendAofPart ini $ zip [1 ..] options

parseArray :: Object -> StatefulParser ModuleParts
parseArray obj = do
    checkType obj $ \case
        Just JS.ArrayTag -> return ()
        _                -> fail "Can't parse this as an array"
    title <- lift $ obj .:? "title"
    let itemField :: String = "items"
    itemsModule <-
        parseDispatch =<<
        lift
            (obj .:? fromString itemField .!=
             case object ["type" .= String "null"] of
                 Object o -> o
                 _        -> undefined)
    let enc = decodeUtf8 . encode $ obj
    case itemsModule ^. declaration of
        (TR.Ref (TR.RefExternalType s tn)) ->
            return $
            ModuleParts
                title
                (Set.singleton s)
                mempty
                (TR.ArrayType (TR.ReferenceToExternalType s tn))
                enc
        (TR.Ref (TR.RefPrimitiveType s)) ->
            return $
            ModuleParts title mempty mempty (TR.ArrayType (TR.ReferenceToPrimitiveType s)) enc
        _local ->
            let tn = U.chooseName itemField (itemsModule ^. jsTitle)
             in return $
                ModuleParts
                    title
                    mempty
                    (Map.singleton itemField itemsModule)
                    (TR.ArrayType (TR.ReferenceToLocalType itemField tn))
                    enc

parseRecordLike :: Object -> StatefulParser ModuleParts
parseRecordLike obj = do
    initialTypeRep <-
        checkType obj $ \case
            Just JS.EnumTag -> return $ TR.SumType mempty
            Just JS.ObjectTag -> do
                additionalProperties <- lift $ obj .:? "additionalProperties"
                return $ TR.ProdType mempty $ fromMaybe False additionalProperties
            _ -> fail "Can't parse this like a record-like type"
    title <- lift $ obj .:? "title"
    (reqs :: [TR.FieldName]) <- lift $ obj .:? "required" .!= mempty
    (properties :: Map TR.FieldName (Object, Bool)) <-
        lift $ checkRequired reqs title =<< obj .:? "properties" .!= mempty
    (parsedProperties :: Map TR.FieldName (ModuleParts, Bool)) <-
        mapM
            (\(obj', req) -> do
                 mp <- parseDispatch obj'
                 return (mp, req))
            properties
    return $
        Map.foldrWithKey
            MP.appendRecord
            (ModuleParts title mempty mempty initialTypeRep (decodeUtf8 . encode $ obj))
            parsedProperties
  where
    checkRequired ::
           [TR.FieldName]
        -> Maybe U.Title
        -> Map TR.FieldName Object
        -> Parser (Map TR.FieldName (Object, Bool))
    checkRequired reqs title properties =
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
                let lcp = LCP.commonPrefix $ splitOn [pathSeparator] . fst <$> incs
                 in addPrefix . mconcat . intersperse [pathSeparator] <$>
                    stripPrefix lcp (splitOn [pathSeparator] y)
      where
        addPrefix :: FilePath -> FilePath
        addPrefix p = U.globalPrefix </> p
    go :: ModuleParts -> ModuleParts
    go mp'
        | TR.ProdType km b <- tr = mpu' & declaration .~ TR.ProdType (mapField <$> km) b
        | TR.SumType km <- tr = mpu' & declaration .~ TR.SumType (fmap (fmap mapField) <$> km)
        | TR.OneOfType km <- tr =
            mpu' & declaration .~ TR.OneOfType (fmap (fmap mapField) <$> km)
        | TR.ArrayType tr' <- tr = mpu' & declaration .~ TR.ArrayType (mapTypeRef tr')
        | TR.NewType tr' <- tr = mpu' & declaration .~ TR.NewType (mapTypeRef tr')
        | TR.Ref nltr <- tr = mpu' & declaration .~ TR.Ref (mapNonLocalRef nltr)
        | TR.AllOfType set <- tr = mpu' & declaration .~ TR.AllOfType (fmap mapTypeRef set)
        | TR.AnyOfType set <- tr = mpu' & declaration .~ TR.AnyOfType (fmap mapTypeRef set)
        | otherwise = mpu'
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
    go (ModuleParts m_s set km tr js) = ModuleParts {..}
      where
        _jsTitle = transform <$> m_s
        _localDeps = Map.mapKeys transform . Map.map go $ km
        _externalDeps = Set.map transform set
        _declaration = transformTypeRep tr
        _json = js
    transformTypeRep :: TR.TypeRep -> TR.TypeRep
    transformTypeRep (TR.ProdType km b) =
        TR.ProdType (Map.mapKeys transform . Map.map transformField $ km) b
    transformTypeRep (TR.SumType km) =
        TR.SumType $ Data.Bifunctor.bimap transform (fmap transformField) <$> km
    transformTypeRep (TR.OneOfType km) =
        TR.OneOfType $ Data.Bifunctor.bimap transform (fmap transformField) <$> km
    transformTypeRep (TR.ArrayType tr') = TR.ArrayType $ transfromTypeRef tr'
    transformTypeRep (TR.NewType tr') = TR.NewType $ transfromTypeRef tr'
    transformTypeRep (TR.Ref (TR.RefExternalType nm tn)) =
        TR.Ref $ TR.RefExternalType (transform nm) (transform tn)
    transformTypeRep (TR.Ref (TR.RefPrimitiveType nm)) = TR.Ref . TR.RefPrimitiveType $ nm
    transformTypeRep (TR.AnyOfType set) = TR.AnyOfType $ fmap transfromTypeRef set
    transformTypeRep (TR.AllOfType set) = TR.AllOfType $ fmap transfromTypeRef set
    transformTypeRep (TR.ConstType v) = TR.ConstType v
    transfromTypeRef :: TR.TypeRef -> TR.TypeRef
    transfromTypeRef (TR.ReferenceToLocalType s tn) =
        TR.ReferenceToLocalType (transform s) (transform tn)
    transfromTypeRef (TR.ReferenceToExternalType s tn) =
        TR.ReferenceToExternalType (transform s) (transform tn)
    transfromTypeRef r = r
    transformField :: TR.Field -> TR.Field
    transformField (TR.Field req tr') = TR.Field req $ transfromTypeRef tr'
