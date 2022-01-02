{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.ConfigGen.ModuleParts where

import           Control.Lens (makeLenses, (^.))
import           Data.Set     (Set)
import qualified Data.Set     as Set
import           GHC.Generics (Generic)

import Data.Yaml (ToJSON)

import           Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.ConfigGen.Traverse.Utils as U
import qualified Data.ConfigGen.TypeRep        as TR

data ModuleParts =
    ModuleParts
        { _jsTitle      :: Maybe String
        , _externalDeps :: Set FilePath
        , _localDeps    :: Map TR.ModuleName ModuleParts
        , _declaration  :: TR.TypeRep
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON)

makeLenses ''ModuleParts

appendAofPart :: (Int, ModuleParts) -> ModuleParts -> ModuleParts
appendAofPart (i, new) ModuleParts {..} =
    let fieldName = U.unnamed ++ show i
     in case new ^. declaration of
            TR.Ref (TR.RefPrimitiveType s) ->
                ModuleParts _jsTitle _externalDeps _localDeps $
                appendToTypeRep _declaration (TR.ReferenceToPrimitiveType s)
            TR.Ref (TR.RefExternalType extName tn) ->
                ModuleParts _jsTitle (Set.insert extName _externalDeps) _localDeps $
                appendToTypeRep _declaration (TR.ReferenceToExternalType extName tn)
            _local ->
                let typename = U.chooseName fieldName (new ^. jsTitle)
                 in ModuleParts _jsTitle _externalDeps (Map.insert fieldName new _localDeps) $
                    appendToTypeRep _declaration (TR.ReferenceToLocalType fieldName typename)
  where
    appendToTypeRep :: TR.TypeRep -> TR.TypeRef -> TR.TypeRep
    appendToTypeRep (TR.AllOfType set) tr = TR.AllOfType $ Set.insert tr set
    appendToTypeRep (TR.AnyOfType set) tr = TR.AnyOfType $ Set.insert tr set
    appendToTypeRep x _                   = x

appendRecord :: TR.FieldName -> (ModuleParts, Bool) -> ModuleParts -> ModuleParts
appendRecord fieldName (record, req) ModuleParts {..} =
    case record ^. declaration of
        TR.Ref (TR.RefPrimitiveType s) ->
            ModuleParts _jsTitle _externalDeps _localDeps $
            appendToTypeRep _declaration fieldName $
            TR.Field req (TR.ReferenceToPrimitiveType s)
        TR.Ref (TR.RefExternalType extName tn) ->
            ModuleParts _jsTitle (Set.insert extName _externalDeps) _localDeps $
            appendToTypeRep _declaration fieldName $
            TR.Field req (TR.ReferenceToExternalType extName tn)
        _local ->
            let typename = U.chooseName fieldName (record ^. jsTitle)
             in ModuleParts _jsTitle _externalDeps (Map.insert fieldName record _localDeps) $
                appendToTypeRep
                    _declaration
                    fieldName
                    (TR.Field req $ TR.ReferenceToLocalType fieldName typename)
  where
    appendToTypeRep :: TR.TypeRep -> TR.FieldName -> TR.Field -> TR.TypeRep
    appendToTypeRep (TR.ProdType km) k tr = TR.ProdType $ Map.insert k tr km
    appendToTypeRep (TR.SumType km) k tr  = TR.SumType $ Map.insert k (TR.SumConstr [tr]) km
    appendToTypeRep (TR.OneOf km) k tr    = TR.OneOf $ Map.insert k (TR.SumConstr [tr]) km
    appendToTypeRep x _ _                 = x
