{-# LANGUAGE RecordWildCards #-}

module Parsing where

import Data.Yaml (FromJSON (parseJSON), Object, Parser, Value (..), (.!=), (.:),
                  (.:?))

import           Data.Aeson.Key    (Key, toString)
import qualified Data.Aeson.KeyMap as KM
import           Data.Aeson.Types  (typeMismatch)

import GHC.SourceGen (HsDecl', HsType', IE', ImportDecl', data', field,
                      recordCon, tuplePromotedTy)

import           Control.Monad  (forM, guard, liftM2)
import           Data.Bifunctor (Bifunctor (bimap))
import qualified Data.List      as L
import           Data.Maybe     (isNothing)
import qualified Data.String    as S
import qualified Data.Text      as T

data FieldTag
    = ObjectTag
    | NumberTag
    | StringTag
    | BooleanTag
    | EnumTag
    | NullTag

typeFieldToFieldTag :: T.Text -> Maybe FieldTag
typeFieldToFieldTag txt =
    case txt of
        "text"   -> Just StringTag
        "object" -> Just ObjectTag
        _        -> Nothing -- todo: add types

newtype RecordTypeStub =
    RecordTypeStub [(String, HsType')]

data YamlModuleStub =
    YamlModuleStub
        { exports         :: Maybe [IE']
        , imports         :: [ImportDecl']
        , content         :: [HsDecl']
        , typeDeclaration :: [HsType']
        }

instance Semigroup YamlModuleStub where
    (<>) l r = YamlModuleStub appExp appImp appCont appFields
      where
        appExp =
            case (exports l, exports r) of
                (Nothing, Nothing) -> Nothing
                (Just l', Nothing) -> Just l'
                (Nothing, Just r') -> Just r'
                (Just l', Just r') -> Just $ l' <> r'
        appImp = imports l <> imports r
        appCont = content l <> content r
        appFields = typeDeclaration l <> typeDeclaration r

instance Monoid YamlModuleStub where
    mempty = YamlModuleStub Nothing [] [] []

instance FromJSON YamlModuleStub where
    parseJSON (Object km) = do
        oType <- km .: "type" :: Parser String
        oName <- km .:? "title" .!= "unnamed" :: Parser String
        required <- km .:? "required" .!= [] :: Parser [Key]
        properties <- km .: "properties" :: Parser Object
        guard $ checkRequired required km
        parseProperties oName properties
      where
        parseProperties :: String -> Object -> Parser YamlModuleStub
        parseProperties oName o = do
            parsedMap <- KM.toAscList <$> forM o (parseJSON :: Value -> Parser YamlModuleStub)
            return .
                (\(YamlModuleStub {..}, fields) ->
                     let curDecl = data' (S.fromString oName) [] [recordCon (S.fromString oName) (bimap S.fromString field <$> fields)] [] -- shuold probably derive some
                         updatedCont = (curDecl : content)
                      in YamlModuleStub exports imports updatedCont undefined) $
                L.foldl'
                    (\(!accMod, !resultingType) (!key, !tt) ->
                         ( accMod <> tt
                         , resultingType <>
                           case typeDeclaration tt of
                               []       -> []
                               typeDecl -> [(toString key, head typeDecl)]))
                    (mempty, [])
                    parsedMap
        checkRequired :: [Key] -> Object -> Bool
        checkRequired (req:reqs) km' = isNothing (KM.lookup req km') && checkRequired reqs km'
        checkRequired [] _ = True
    parseJSON (Array vec) = do
        constMod <- L.foldl' (\acc new -> liftM2 (<>) acc (parseJSON new)) mempty vec
        let tupled_dec = tuplePromotedTy $ typeDeclaration constMod
        return $ YamlModuleStub (exports constMod) (imports constMod) (content constMod) [tupled_dec]
    parseJSON invalid = typeMismatch "YamlMuduleStub" invalid
