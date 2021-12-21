{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
import qualified Data.Text      as T

import qualified Data.ConfigGen.JSTypes     as JS
import qualified Data.ConfigGen.Parsing.LCP as LCP
import           Data.ConfigGen.TypeRep     (ModuleParts (..))
import qualified Data.ConfigGen.TypeRep     as TR
import           Data.List                  (intersperse, stripPrefix)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           GHC.Generics               (Generic)
import           System.FilePath            (takeBaseName, takeFileName)
import           Util                       (split)

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

type Title = String

type Origin = String

type Properties = Object

-- parse file, collect its dependencies
-- extract path of processed file, append it to external dependecies of itself
-- discard main type in Parser result, use include map as start to next file
-- check if it is there at first, and if not process it as previouse one
-- in result there will be update keymap of external depencies of both files
-- continue as such until all files are processed
-- I hope that in result there will be no unnecacry include processing and nothing will be lost
-- This is note for future me, who will continue to work on it on mondaytype ParserResultFromDir = Reader (NonEmpty FilePath) (Either String ParserResult)
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

expectObject :: forall a. String -> (Object -> StatefulParser a) -> Value -> StatefulParser a
expectObject _ f (Object o) = f o
expectObject msg _ v        = lift $ prependFailure msg (typeMismatch "Object" v)

expectString :: forall a. String -> (T.Text -> StatefulParser a) -> Value -> StatefulParser a
expectString _ f (String o) = f o
expectString msg _ v        = lift $ prependFailure msg (typeMismatch "String" v)

expectArray :: forall a. String -> (Array -> StatefulParser a) -> Value -> StatefulParser a
expectArray _ f (Array o) = f o
expectArray msg _ v       = lift $ prependFailure msg (typeMismatch "Array" v)

parseDispatch :: Object -> StatefulParser ModuleParts
parseDispatch obj = do
    jsTypeTag <- lift $ failOrJsTypeTag
    maybeOrigin <- lift $ obj .:? "haskell/origin"
    typeInfo <- lift $ obj .:? "haskell/type-info"
    title <- lift $ titleFromOrigin maybeOrigin <$> obj .:? "title"
    retrieveCachedInclude title maybeOrigin $
        case jsTypeTag of
            JS.Prim tag -> parsePrimitve tag title typeInfo
        -- here will go things for allOf, anyOf, oneOf
            JS.Rec tag  -> parseRecordLike obj tag title
            JS.ArrayTag -> parseArray obj title
  where
    titleFromOrigin :: Maybe Origin -> Maybe Title -> Maybe Title
    titleFromOrigin _ (Just title) = Just $ title
    titleFromOrigin origin Nothing   = origin
    failOrJsTypeTag :: Parser JS.TypeTag
    failOrJsTypeTag = do
        (maybeJsTypeTag :: Maybe JS.TypeTag) <-
            JS.parseTypeTag <$> (obj .:? "type" .!= "object")
        maybe (fail $ "Found incorect type here: " ++ show obj) return $ maybeJsTypeTag
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
            return . ModuleParts title mempty mempty . TR.Ref . TR.RefExternalType $ origin

checkAdditionalPropertiesFlag :: Object -> Parser ()
checkAdditionalPropertiesFlag obj = do
    (maybeAdditionalProps :: Maybe Bool) <- obj .:? "additionalProperties"
    let invalidAdditionalProps =
            "additionalProperties must be set to False:" ++ show obj ++ "\n"
    maybe
        (fail $ invalidAdditionalProps ++ "They do not exist. This fact implies True")
        (\case
             True  -> fail invalidAdditionalProps
             False -> return ())
        maybeAdditionalProps

parseArray :: Object -> Maybe Title -> StatefulParser ModuleParts
parseArray obj maybeTitle = do
    let itemField :: String = "items"
    -- todo: think about newtype
    items <- parseDispatch =<< lift (obj .: fromString itemField)
    case items ^. declaration of
        (TR.Ref (TR.RefExternalType s)) ->
            return $
            ModuleParts maybeTitle (Set.singleton s) mempty $
            (TR.ArrayType (TR.ReferenceToExternalType s))
        (TR.Ref (TR.RefPrimitiveType s)) ->
            return $
            ModuleParts maybeTitle mempty mempty $ TR.ArrayType (TR.ReferenceToPrimitiveType s)
        _local ->
            return $
            ModuleParts maybeTitle mempty (Map.singleton itemField items) $
            TR.ArrayType (TR.ReferenceToLocalType itemField)

parsePrimitve :: JS.PrimitiveTag -> Maybe Title -> Maybe TypeInfo -> StatefulParser ModuleParts
parsePrimitve typeTag maybeTitle tInfo = do
    let newtypeWrapper =
            case maybeTitle of
                Just s  -> (\nlr -> TR.NewType s . TR.ExtRef $ nlr)
                Nothing -> (\nlr -> TR.Ref nlr)
    return . ModuleParts maybeTitle mempty mempty . newtypeWrapper . TR.RefPrimitiveType $
        case typeTag of
            JS.PrimStringTag -> fromMaybe "Data.Text.Text" tInfo
            JS.PrimNumberTag -> fromMaybe "Int" tInfo
            JS.PrimBoolTag   -> "Bool"
            JS.PrimNullTag   -> "()"

parseInclude ::
       Object -> Origin -> Maybe Title -> JS.RecordLikeTag -> StatefulParser ModuleParts
parseInclude obj origin title typeTag = do
    maybeCachedIncldue <- fmap (Map.lookup origin) $ get <&> _cachedIncludes
    case maybeCachedIncldue of
        Nothing -> do
            res <- parseRecordLike obj typeTag title
            modify $ cachedIncludes %~ Map.insert origin res
        Just _ -> return ()
    return . ModuleParts title mempty mempty . TR.Ref . TR.RefExternalType $ origin

parseRecordLike :: Object -> JS.RecordLikeTag -> Maybe Title -> StatefulParser ModuleParts
parseRecordLike obj typeTag title
    -- lift $ checkAdditionalPropertiesFlag obj
 = do
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
    appendRecord k record ModuleParts {..} =
        let chosenName = chooseName k $ record ^. jsTitle
            chooseName :: TR.FieldName -> Maybe Title -> TR.FieldName
            chooseName k' m_s = fromString $ fromMaybe k' m_s
         in case record ^. declaration of
                TR.Ref (TR.RefPrimitiveType s) ->
                    ModuleParts _jsTitle _externalDeps _localDeps $
                    TR.appendToTypeRep _declaration chosenName $ TR.ReferenceToPrimitiveType s
                TR.Ref (TR.RefExternalType extName) ->
                    ModuleParts _jsTitle (Set.insert extName _externalDeps) _localDeps $
                    TR.appendToTypeRep _declaration k $ TR.ReferenceToExternalType extName
                _local ->
                    ModuleParts
                        _jsTitle
                        _externalDeps
                        (Map.insert chosenName record _localDeps) $
                    TR.appendToTypeRep _declaration k (TR.ReferenceToLocalType chosenName)
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
                let lcp = LCP.commonPrefix $ split '/' . fst <$> incs
                 in mconcat . intersperse "/" <$> stripPrefix lcp (split '/' y)
    go :: ModuleParts -> ModuleParts
    go mp'
        | TR.ProdType km <- tr = mpu' & declaration .~ (TR.ProdType $ mapTypeRef <$> km)
        | TR.SumType km <- tr = mpu' & declaration .~ (TR.SumType $ mapTypeRef <$> km)
        | TR.ArrayType tr' <- tr = mpu' & declaration .~ (TR.ArrayType $ mapTypeRef tr')
        | TR.NewType s tr' <- tr = mpu' & declaration .~ (TR.NewType s $ mapTypeRef tr')
        | TR.Ref nltr <- tr = mpu' & declaration .~ (TR.Ref $ mapNonLocalRef nltr)
      where
        tr = _declaration mp'
        mpu = mp' & externalDeps %~ Set.map (\x -> fromJust $ stripPathPrefix x)
        mpu' = mpu & localDeps %~ fmap go
    mapNonLocalRef :: TR.NonLocalRef -> TR.NonLocalRef
    mapNonLocalRef tr
        | TR.RefExternalType s <- tr = TR.RefExternalType $ fromJust $ stripPathPrefix s
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
    transformTypeRep (TR.NewType s tr') = TR.NewType (transform s) $ transfromTypeRef tr'
    transformTypeRep (TR.Ref (TR.RefExternalType nm)) =
        TR.Ref . TR.RefExternalType $ transform nm
    transformTypeRep (TR.Ref (TR.RefPrimitiveType nm)) =
        TR.Ref . TR.RefPrimitiveType $ transform nm
    transfromTypeRef :: TR.TypeRef -> TR.TypeRef
    transfromTypeRef (TR.ReferenceToLocalType s) = TR.ReferenceToLocalType $ transform s
    transfromTypeRef (TR.ReferenceToExternalType s) = TR.ReferenceToExternalType $ transform s
    transfromTypeRef (TR.ReferenceToPrimitiveType s) =
        TR.ReferenceToPrimitiveType $ transform s
    transform :: String -> String
    transform nm = map l nm
      where
        l '-' = '_'
        l s   = s
