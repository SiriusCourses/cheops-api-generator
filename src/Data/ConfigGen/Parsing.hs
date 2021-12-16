{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.ConfigGen.Parsing where

import Control.Lens               (makeLenses, (%~), (&), (.~), (<&>), (^.))
import Control.Monad.State.Strict (MonadTrans (lift), StateT (runStateT), get, modify)

import qualified Data.Aeson.Key    as K (toString)
import           Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import           Data.Aeson.Types  (Key, prependFailure, typeMismatch)
import           Data.Yaml         (Array, FromJSON (..), Object, Parser, ToJSON, Value (..),
                                    (.!=), (.:), (.:?))

import qualified Data.Bifunctor
import           Data.Maybe     (fromJust, fromMaybe)
import qualified Data.Set       as Set
import           Data.String    (IsString (fromString))
import qualified Data.Text      as T

import qualified Data.ConfigGen.JSTypes     as JS
import qualified Data.ConfigGen.Parsing.LCP as LCP
import           Data.ConfigGen.TypeRep     (ModuleParts (..))
import qualified Data.ConfigGen.TypeRep     as TR
import           Data.List                  (stripPrefix)
import           GHC.Generics               (Generic)
import           System.FilePath            (takeFileName)
import           Util                       (split)

newtype ParserState =
    ParserState
        { _cachedIncludes :: KeyMap ModuleParts
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

instance FromJSON ParserResult where
    parseJSON (Object obj) =
        postprocessParserResult . makeParserResult <$> runStateT (parseDispatch obj) mempty
      where
        makeParserResult :: (ModuleParts, ParserState) -> ParserResult
        makeParserResult (mainType, ps) =
            let deps = Data.Bifunctor.first K.toString <$> KM.toAscList (_cachedIncludes ps)
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
        (maybeJsTypeTag :: Maybe JS.TypeTag) <- JS.parseTypeTag <$> (obj .: "type")
        maybe (fail $ "Found incorect type here: " ++ show obj) return $ maybeJsTypeTag
    retrieveCachedInclude ::
           Maybe Title
        -> Maybe Origin
        -> StatefulParser ModuleParts -- how to cache if not yet cached
        -> StatefulParser ModuleParts
    retrieveCachedInclude title m_origin k =
        (\t -> maybe k t m_origin) $ \origin -> do
            maybeCachedIncldue <-
                fmap (KM.lookup . fromString $ origin) $ get <&> _cachedIncludes
            case maybeCachedIncldue of
                Nothing -> do
                    res <- k
                    modify $ cachedIncludes %~ KM.insert (fromString origin) res
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
    let itemField = "items"
    -- todo: think about newtype
    items <- parseDispatch =<< lift (obj .: itemField)
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
            ModuleParts maybeTitle mempty (KM.singleton itemField items) $
            TR.ArrayType (TR.ReferenceToLocalType $ K.toString itemField)

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

parseInclude :: Object -> Key -> Maybe Title -> JS.RecordLikeTag -> StatefulParser ModuleParts
parseInclude obj origin title typeTag = do
    maybeCachedIncldue <- fmap (KM.lookup origin) $ get <&> _cachedIncludes
    case maybeCachedIncldue of
        Nothing -> do
            res <- parseRecordLike obj typeTag title
            modify $ cachedIncludes %~ KM.insert origin res
        Just _ -> return ()
    return . ModuleParts title mempty mempty . TR.Ref . TR.RefExternalType $ K.toString origin

parseRecordLike :: Object -> JS.RecordLikeTag -> Maybe Title -> StatefulParser ModuleParts
parseRecordLike obj typeTag title
    -- lift $ checkAdditionalPropertiesFlag obj
 = do
    (reqs :: [Key]) <- lift $ obj .:? "required" .!= mempty
    (properties :: Object) <- lift $ checkRequired reqs =<< obj .:? "properties" .!= mempty
    (parsedProperties :: KeyMap ModuleParts) <-
        mapM (expectObject "failed to parse property to Object" parseDispatch) properties
    let initialTypeRep =
            case typeTag of
                JS.RecEnumTag   -> TR.SumType mempty
                JS.RecObjectTag -> TR.ProdType mempty
    return $
        KM.foldrWithKey
            appendRecord
            (ModuleParts title mempty mempty initialTypeRep)
            parsedProperties
  where
    appendRecord :: Key -> ModuleParts -> ModuleParts -> ModuleParts
    appendRecord k record ModuleParts {..} =
        let chosenName = chooseName k $ record ^. jsTitle
            chooseName :: Key -> Maybe String -> Key
            chooseName k' m_s = fromString $ fromMaybe (K.toString k') m_s
         in case record ^. declaration of
                TR.Ref (TR.RefPrimitiveType s) ->
                    ModuleParts _jsTitle _externalDeps _localDeps $
                    TR.appendToTypeRep _declaration chosenName $ TR.ReferenceToPrimitiveType s
                TR.Ref (TR.RefExternalType extName) ->
                    ModuleParts _jsTitle (Set.insert extName _externalDeps) _localDeps $
                    TR.appendToTypeRep _declaration k $ TR.ReferenceToExternalType extName
                _local ->
                    ModuleParts _jsTitle _externalDeps (KM.insert chosenName record _localDeps) $
                    TR.appendToTypeRep
                        _declaration
                        k
                        (TR.ReferenceToLocalType . K.toString $ chosenName)
    checkRequired :: [Key] -> Properties -> Parser Object
    checkRequired reqs properties =
        if all (`KM.member` properties) reqs
            then return properties
            else fail $ "There are some fields that are required but not present: " ++ show obj

postprocessParserResult :: ParserResult -> ParserResult
postprocessParserResult pr@(ParserResult _ []) = pr
postprocessParserResult (ParserResult s [x]) =
    ParserResult s $ [Data.Bifunctor.first takeFileName x]
postprocessParserResult (ParserResult mp incs) =
    ParserResult (go mp) $ Data.Bifunctor.bimap (fromJust . stripPathPrefix) go <$> incs
  where
    lcp = LCP.commonPrefix $ split '/' . fst <$> incs
    stripPathPrefix y = mconcat <$> stripPrefix lcp (split '/' y)
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
