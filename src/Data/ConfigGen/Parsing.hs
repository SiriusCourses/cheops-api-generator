{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.ConfigGen.Parsing where

import Control.Lens               (makeLenses, (%~), (<&>), (^.))
import Control.Monad.State.Strict (MonadTrans (lift), StateT (runStateT), get, modify)

import qualified Data.Aeson.Key    as K (toString)
import           Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import           Data.Aeson.Types  (Key, prependFailure, typeMismatch)
import           Data.Yaml         (Array, FromJSON (..), Object, Parser, Value (..), (.!=),
                                    (.:), (.:?), ToJSON)

import qualified Data.Bifunctor
import           Data.Maybe     (fromMaybe)
import           Data.Set       (Set)
import qualified Data.Set       as Set
import           Data.String    (IsString (fromString))
import qualified Data.Text      as T

import qualified Data.ConfigGen.JSTypes as JS
import           Data.ConfigGen.TypeRep (ModuleParts (..))
import qualified Data.ConfigGen.TypeRep as TR
import GHC.Generics (Generic)

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

type Properties = Object

instance FromJSON ParserResult where
    parseJSON (Object obj) = makeParserResult <$> runStateT (parseDispatch obj) mempty
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

defaultExternalDeps :: Set TR.ModuleName
defaultExternalDeps = Set.fromList ["GHC.Types", "GHC.Int", "Data.Text"]

parseDispatch :: Object -> StatefulParser ModuleParts
parseDispatch obj = do
    (maybeJsTypeTag :: Maybe JS.TypeTag) <- lift $ JS.parseTypeTag <$> (obj .: "type")
    (maybeOrigin :: Maybe String) <- lift $ obj .:? "haskell/origin"
    jsTypeTag <- maybe (fail $ "Found incorect type here: " ++ show obj) return maybeJsTypeTag
    title <- lift $ parseTitle obj
    typeInfo <- lift $ parseTitle obj
    case (jsTypeTag, maybeOrigin) of
        (JS.Prim tag, _)          -> lift $ parsePrimitve tag title typeInfo
        -- here will go things for allOf, anyOf, oneOf
        (JS.Rec tag, Nothing)     -> parseRecordLike obj tag title
        (JS.Rec tag, Just origin) -> parseInclude obj (fromString origin) tag
        (JS.ArrayTag, arrTitle)   -> parseArray obj arrTitle

parseTypeInfo :: Object -> Parser (Maybe String)
parseTypeInfo obj = obj .:? "haskell/type-info"

parseTitle :: Object -> Parser (Maybe String)
parseTitle obj = obj .:? "title"

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
    let newtypeWrapper = maybe id TR.NewType maybeTitle
    let itemField = "items"
    items <- parseDispatch =<< lift (obj .: itemField)
    case items ^. declaration of
        (TR.Ref (TR.RefExternalType s)) ->
            return $
            ModuleParts maybeTitle (Set.singleton s) mempty $
            newtypeWrapper (TR.ArrayType (TR.ReferenceToExternalType s))
        (TR.Ref (TR.RefPrimitiveType s)) ->
            return $
            ModuleParts maybeTitle mempty mempty $ TR.ArrayType (TR.ReferenceToPrimitiveType s)
        _local ->
            return $
            ModuleParts maybeTitle mempty (KM.singleton itemField items) $
            TR.ArrayType (TR.ReferenceToLocalType $ K.toString itemField)

parsePrimitve :: JS.PrimitiveTag -> Maybe Title -> Maybe TypeInfo -> Parser ModuleParts
parsePrimitve typeTag maybeTitle tInfo = do
    let newtypeWrapper = maybe TR.Ref (\s -> TR.NewType s . TR.Ref) maybeTitle
    return .
        ModuleParts Nothing defaultExternalDeps mempty . newtypeWrapper . TR.RefPrimitiveType $
        case typeTag of
            JS.PrimStringTag -> fromMaybe "Data.Text.Text" tInfo
            JS.PrimNumberTag -> fromMaybe "Int" tInfo
            JS.PrimBoolTag   -> "Bool"
            JS.PrimNullTag   -> "()"

parseInclude :: Object -> Key -> JS.RecordLikeTag -> StatefulParser ModuleParts
parseInclude obj origin typeTag = do
    maybeCachedIncldue <- fmap (KM.lookup origin) $ get <&> _cachedIncludes
    title <- lift $ parseTitle obj
    case maybeCachedIncldue of
        Nothing -> do
            res <- parseRecordLike obj typeTag title
            modify $ cachedIncludes %~ KM.insert origin res
        Just _ -> return ()
    return . ModuleParts Nothing mempty mempty . TR.Ref . TR.RefExternalType $
        K.toString origin

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
{-
ParserResult {
    mainType = ModuleParts {
        _jsTitle = Just "LatexRequest",
        _externalDeps = fromList [
            "/Users/frogofjuly/Documents/Haskell/src/config-generation/models/./ratio.yaml"
        ],
        _localDeps = fromList [
            ("glossary",ModuleParts {
                _jsTitle = Nothing,
                _externalDeps = fromList [
                    "/Users/frogofjuly/Documents/Haskell/src/config-generation/models/./ratio.yaml"
                ],
                _localDeps = fromList [
                    ("som\1091thingelse",ModuleParts {
                        _jsTitle = Nothing,
                        _externalDeps = fromList [],
                        _localDeps = fromList [],
                        _declaration = SumType (fromList [
                            ("another",ExtRef (RefPrimitiveType "Data.Text.Text")),
                            ("one",ExtRef (RefPrimitiveType "Int"))
                        ])
                    })
                ],
                _declaration = SumType (fromList [
                    ("name",ExtRef (RefPrimitiveType "Data.Text.Text")),
                    ("nested_ratio",ExtRef (RefExternalType "/Users/frogofjuly/Documents/Haskell/src/config-generation/models/./ratio.yaml")),
                    ("size'",ExtRef (RefPrimitiveType "Int")),
                    ("som\1091thingelse",ReferenceToLocalType "som\1091thingelse")
                ])
            })
        ],
        _declaration = SumType (fromList [
            ("glossary",ReferenceToLocalType "glossary"),
            ("ratio",ExtRef (RefExternalType "/Users/frogofjuly/Documents/Haskell/src/config-generation/models/./ratio.yaml"))
        ])
    },
    deps = [
        ("/Users/frogofjuly/Documents/Haskell/src/config-generation/models/./ratio.yaml", ModuleParts {
            _jsTitle = Just "ratio",
            _externalDeps = fromList [],
            _localDeps = fromList [
                ("lala",ModuleParts {
                    _jsTitle = Just "lala",
                    _externalDeps = fromList [],
                    _localDeps = fromList [],
                    _declaration = SumType (fromList [
                        ("lenght",ExtRef (RefPrimitiveType "Data.Text.Text")),
                        ("size",ExtRef (RefPrimitiveType "Int"))
                    ])
                })
            ],
            _declaration = SumType (fromList [
                ("denum",ExtRef (RefPrimitiveType "Int")),
                ("lala",ReferenceToLocalType "lala"),
                ("num",ExtRef (RefPrimitiveType "Int")
            )]
        )})
    ]
}
-}
{-
ParserResult {
    mainType = ModuleParts {
        _jsTitle = Just "TokenResponseToken",
        _externalDeps = fromList [],
        _localDeps = fromList [],
        _declaration = SumType (fromList [
            ("token",ExtRef (RefPrimitiveType "Data.Text.Text"))
        ])
    },
    deps = []}
-}
{-
ParserResult {
    mainType = ModuleParts {
        _jsTitle = Just "DownloaderJobResponse", 
        _externalDeps = fromList [], 
        _localDeps = fromList [
            ("DownloaderJobBatchContainer",ModuleParts {
                _jsTitle = Just "DownloaderJobBatchContainer", 
                _externalDeps = fromList [], 
                _localDeps = fromList [], 
                _declaration = SumType (fromList [
                    ("id",ExtRef (RefPrimitiveType "Int"))
                ])
            }),
            ("dialogue",ModuleParts {
                _jsTitle = Nothing, 
                _externalDeps = fromList [], 
                _localDeps = fromList [
                    ("items",ModuleParts {
                        _jsTitle = Nothing, 
                        _externalDeps = fromList [], 
                        _localDeps = fromList [], 
                        _declaration = SumType (fromList [])
                    })
                ], 
                _declaration = ArrayType (ReferenceToLocalType "items")
            }),
            ("errors",ModuleParts {
                _jsTitle = Nothing, 
                _externalDeps = fromList [
                    "/Users/frogofjuly/Documents/Haskell/src/config-generation/api/smt-api-spec/api/common/schema/downloader/../response/error-object.yaml"
                    ], 
                _localDeps = fromList [], 
                _declaration = ArrayType (ExtRef (RefExternalType "/Users/frogofjuly/Documents/Haskell/src/config-generation/api/smt-api-spec/api/common/schema/downloader/../response/error-object.yaml"))
            }),
            ("warnings",ModuleParts {
                _jsTitle = Nothing, 
                _externalDeps = fromList [
                    "/Users/frogofjuly/Documents/Haskell/src/config-generation/api/smt-api-spec/api/common/schema/downloader/../response/error-object.yaml"
                    ], 
                _localDeps = fromList [], 
                _declaration = ArrayType (ExtRef (RefExternalType "/Users/frogofjuly/Documents/Haskell/src/config-generation/api/smt-api-spec/api/common/schema/downloader/../response/error-object.yaml"))
            })
        ], 
        _declaration = SumType (fromList [
            ("dialogue",ReferenceToLocalType "dialogue"),
            ("errors",ReferenceToLocalType "errors"),
            ("success",ReferenceToLocalType "DownloaderJobBatchContainer"),
            ("warnings",ReferenceToLocalType "warnings")
        ])
    }, 
    deps = [
        ("/Users/frogofjuly/Documents/Haskell/src/config-generation/api/smt-api-spec/api/common/schema/downloader/../response/error-object.yaml",ModuleParts {
            _jsTitle = Just "ErrorItem", 
            _externalDeps = fromList [], 
            _localDeps = fromList [
                ("params",ModuleParts {
                    _jsTitle = Nothing, 
                    _externalDeps = fromList [], 
                    _localDeps = fromList [], 
                    _declaration = SumType (fromList [])
                })
            ], 
            _declaration = SumType (fromList [
                ("key",ExtRef (RefPrimitiveType "Data.Text.Text")),
                ("message",ExtRef (RefPrimitiveType "Data.Text.Text")),
                ("params",ReferenceToLocalType "params")
            ])
        })
    ]
}
-}