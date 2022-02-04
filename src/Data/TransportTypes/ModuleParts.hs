{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.TransportTypes.ModuleParts where

import           Control.Lens (makeLenses, (^.))
import           Data.Set     (Set)
import qualified Data.Set     as Set
import           Data.Text    (Text)
import           GHC.Generics (Generic)

import Data.Yaml (ToJSON)

import           Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.TransportTypes.CodeGen.NamingUtils as U
import qualified Data.TransportTypes.TypeRep             as TR

data ModuleParts =
    ModuleParts
        { _jsTitle      :: Maybe String
        , _externalDeps :: Set FilePath
        , _localDeps    :: Map TR.ModuleName ModuleParts
        , _declaration  :: TR.TypeRep
        , _json         :: Text
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON)

makeLenses ''ModuleParts

appendAofPart :: (Int, ModuleParts) -> ModuleParts -> ModuleParts
appendAofPart (i, new) ModuleParts {..} =
    let fieldName = U.unnamed ++ show i
     in case new ^. declaration of
            TR.Ref (TR.RefPrimitiveType s) ->
                ModuleParts
                    _jsTitle
                    _externalDeps
                    _localDeps
                    (appendToTypeRep _declaration (TR.ReferenceToPrimitiveType s))
                    _json
            TR.Ref (TR.RefExternalType extName tn) ->
                ModuleParts
                    _jsTitle
                    (Set.insert extName _externalDeps)
                    _localDeps
                    (appendToTypeRep _declaration (TR.ReferenceToExternalType extName tn))
                    _json
            _local ->
                let typename = U.chooseName fieldName (new ^. jsTitle)
                 in ModuleParts
                        _jsTitle
                        _externalDeps
                        (Map.insert fieldName new _localDeps)
                        (appendToTypeRep
                             _declaration
                             (TR.ReferenceToLocalType fieldName typename))
                        _json
  where
    appendToTypeRep :: TR.TypeRep -> TR.TypeRef -> TR.TypeRep
    appendToTypeRep (TR.AllOfType set) tr = TR.AllOfType $ tr : set
    appendToTypeRep (TR.AnyOfType set) tr = TR.AnyOfType $ tr : set
    appendToTypeRep x _                   = x

appendRecord :: TR.FieldName -> (ModuleParts, Bool) -> ModuleParts -> ModuleParts
appendRecord fieldName (record, req) ModuleParts {..} =
    case record ^. declaration of
        TR.Ref (TR.RefPrimitiveType s) ->
            ModuleParts
                _jsTitle
                _externalDeps
                _localDeps
                (appendToTypeRep _declaration fieldName $
                 TR.Field req (TR.ReferenceToPrimitiveType s))
                _json
        TR.Ref (TR.RefExternalType extName tn) ->
            ModuleParts
                _jsTitle
                (Set.insert extName _externalDeps)
                _localDeps
                (appendToTypeRep _declaration fieldName $
                 TR.Field req (TR.ReferenceToExternalType extName tn))
                _json
        _local ->
            let typename = U.chooseName fieldName (record ^. jsTitle)
             in ModuleParts
                    _jsTitle
                    _externalDeps
                    (Map.insert fieldName record _localDeps)
                    (appendToTypeRep
                         _declaration
                         fieldName
                         (TR.Field req $ TR.ReferenceToLocalType fieldName typename))
                    _json
  where
    appendToTypeRep :: TR.TypeRep -> TR.FieldName -> TR.Field -> TR.TypeRep
    appendToTypeRep (TR.ProdType km b) k tr = TR.ProdType (Map.insert k tr km) b
    appendToTypeRep (TR.SumType km) k tr    = TR.SumType $ (k, TR.SumConstr [tr]) : km
    appendToTypeRep (TR.OneOfType km) k tr  = TR.OneOfType $ (k, TR.SumConstr [tr]) : km
    appendToTypeRep x _ _                   = x
