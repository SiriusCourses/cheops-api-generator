{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.ConfigGen.Parsing where

import Control.Lens               (makeLenses, (%~), (&), (.~), (<&>), (^.))
import Control.Monad.State.Strict (MonadTrans (lift), StateT (runStateT), get, modify)

import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Yaml        (FromJSON (..), Object, Parser, ToJSON, Value (..), (.!=), (.:), (.:?))

import qualified Data.Bifunctor
import           Data.Maybe     (fromJust, fromMaybe)
import qualified Data.Set       as Set
import           Data.String    (IsString (fromString))

import qualified Data.ConfigGen.JSTypes        as JS
import qualified Data.ConfigGen.Parsing.LCP    as LCP
import           Data.ConfigGen.Traverse.Utils (Title)
import qualified Data.ConfigGen.Traverse.Utils as U
import           Data.ConfigGen.TypeRep        (ModuleParts (..))
import qualified Data.ConfigGen.TypeRep        as TR
import           Data.List                     (intersperse, stripPrefix)
import           Data.List.Utils               (split)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           GHC.Generics                  (Generic)
import           System.FilePath               (pathSeparator, takeBaseName)

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
        failMsg =
            "parsing of JSON-scheme failed, probably encoured something wrong or somewhere wrong"

parseDispatch :: Object -> StatefulParser ModuleParts
parseDispatch obj = do
    jsTypeTag <- lift failOrJsTypeTag
    maybeOrigin <- lift $ obj .:? "haskell/origin"
    typeInfo <- lift $ obj .:? "haskell/type-info"
    title <- lift $ obj .:? "title"
    retrieveCachedInclude title maybeOrigin $
        case jsTypeTag of
            JS.Prim tag -> parsePrimitve tag title typeInfo
        -- here will go things for allOf, anyOf, oneOf
            JS.Rec tag  -> parseRecordLike obj tag title
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
            -- should here be non empty external dependecy??
            return . ModuleParts title mempty mempty . TR.Ref $
                TR.RefExternalType origin (U.typeNameFromAbsolutePath origin title)

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

parsePrimitve :: JS.PrimitiveTag -> Maybe Title -> Maybe TypeInfo -> StatefulParser ModuleParts
parsePrimitve typeTag maybeTitle tInfo = do
    let newtypeWrapper =
            case maybeTitle of
                Just _  -> TR.NewType . TR.ExtRef
                Nothing -> TR.Ref
    return . ModuleParts maybeTitle mempty mempty . newtypeWrapper . TR.RefPrimitiveType $
        case typeTag of
            JS.PrimStringTag -> fromMaybe "Data.Text.Text" tInfo
            JS.PrimNumberTag -> fromMaybe "Int" tInfo
            JS.PrimBoolTag   -> "Bool"
            JS.PrimNullTag   -> "()"

parseRecordLike :: Object -> JS.RecordLikeTag -> Maybe Title -> StatefulParser ModuleParts
parseRecordLike obj typeTag title = do
    (reqs :: [TR.FieldName]) <- lift $ obj .:? "required" .!= mempty
    (properties :: Map TR.FieldName Object) <-
        lift $ checkRequired reqs =<< obj .:? "properties" .!= mempty
    (parsedProperties :: Map TR.FieldName ModuleParts) <- mapM parseDispatch properties
    let initialTypeRep =
            case typeTag of
                JS.RecEnumTag   -> TR.SumType mempty
                JS.RecObjectTag -> TR.ProdType mempty
    return $
        Map.foldrWithKey
            appendRecord
            (ModuleParts title mempty mempty initialTypeRep)
            parsedProperties
  where
    appendRecord :: TR.FieldName -> ModuleParts -> ModuleParts -> ModuleParts
    appendRecord fieldName record ModuleParts {..} =
        case record ^. declaration of
            TR.Ref (TR.RefPrimitiveType s) ->
                ModuleParts _jsTitle _externalDeps _localDeps $
                TR.appendToTypeRep _declaration fieldName $ TR.ReferenceToPrimitiveType s
            TR.Ref (TR.RefExternalType extName tn) ->
                ModuleParts _jsTitle (Set.insert extName _externalDeps) _localDeps $
                TR.appendToTypeRep _declaration fieldName $
                TR.ReferenceToExternalType extName tn
            _local ->
                let typename = U.chooseName fieldName (record ^. jsTitle)
                 in ModuleParts _jsTitle _externalDeps (Map.insert fieldName record _localDeps) $
                    TR.appendToTypeRep
                        _declaration
                        fieldName
                        (TR.ReferenceToLocalType fieldName typename)
    checkRequired ::
           [TR.FieldName] -> Map TR.FieldName Object -> Parser (Map TR.FieldName Object)
    checkRequired reqs properties =
        if all (`Map.member` properties) reqs
            then return properties
            else fail $ "There are some fields that are required but not present: " ++ show obj

postprocessParserResult :: ParserResult -> ParserResult
postprocessParserResult (ParserResult mp incs) =
    ParserResult (go mp) $ Data.Bifunctor.bimap (fromJust . stripPathPrefix) go <$> incs
  where
    stripPathPrefix y =
        case incs of
            [] -> Just $ takeBaseName y
            [_] -> Just $ takeBaseName y
            _ ->
                let lcp = LCP.commonPrefix $ split [pathSeparator] . fst <$> incs
                 in mconcat . intersperse [pathSeparator] <$>
                    stripPrefix lcp (split [pathSeparator] y)
    go :: ModuleParts -> ModuleParts
    go mp'
        | TR.ProdType km <- tr = mpu' & declaration .~ TR.ProdType (mapTypeRef <$> km)
        | TR.SumType km <- tr = mpu' & declaration .~ TR.SumType (mapTypeRef <$> km)
        | TR.ArrayType tr' <- tr = mpu' & declaration .~ TR.ArrayType (mapTypeRef tr')
        | TR.NewType tr' <- tr = mpu' & declaration .~ TR.NewType (mapTypeRef tr')
        | TR.Ref nltr <- tr = mpu' & declaration .~ TR.Ref (mapNonLocalRef nltr)
      where
        tr = _declaration mp'
        mpu = mp' & externalDeps %~ Set.map (fromJust . stripPathPrefix)
        mpu' = mpu & localDeps %~ fmap go
    mapNonLocalRef :: TR.NonLocalRef -> TR.NonLocalRef
    mapNonLocalRef tr
        | TR.RefExternalType s tn <- tr = TR.RefExternalType (fromJust $ stripPathPrefix s) tn
        | otherwise = tr
    mapTypeRef :: TR.TypeRef -> TR.TypeRef
    mapTypeRef (TR.ExtRef nlr) = TR.ExtRef $ mapNonLocalRef nlr
    mapTypeRef r               = r

replaceDashesWithUnderscores :: ParserResult -> ParserResult
replaceDashesWithUnderscores (ParserResult mp deps) =
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
        TR.ProdType $ Map.mapKeys transform . Map.map transfromTypeRef $ km
    transformTypeRep (TR.SumType km) =
        TR.SumType $ Map.mapKeys transform . Map.map transfromTypeRef $ km
    transformTypeRep (TR.ArrayType tr') = TR.ArrayType $ transfromTypeRef tr'
    transformTypeRep (TR.NewType tr') = TR.NewType $ transfromTypeRef tr'
    transformTypeRep (TR.Ref (TR.RefExternalType nm tn)) =
        TR.Ref $ TR.RefExternalType (transform nm) (transform tn)
    transformTypeRep (TR.Ref (TR.RefPrimitiveType nm)) =
        TR.Ref . TR.RefPrimitiveType $ transform nm
    transfromTypeRef :: TR.TypeRef -> TR.TypeRef
    transfromTypeRef (TR.ReferenceToLocalType s tn) =
        TR.ReferenceToLocalType (transform s) (transform tn)
    transfromTypeRef (TR.ReferenceToExternalType s tn) =
        TR.ReferenceToExternalType (transform s) (transform tn)
    transfromTypeRef r = r
    transform :: String -> String
    transform nm = map l nm
      where
        l '-' = '_'
        l s   = s
