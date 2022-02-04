{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Data.TransportTypes.ModuleParts

ModuleParts is a node that represent ONE type from yaml schema.
It also represent its connection with its subschemas, type structure and other usefull information.
-}
module Data.TransportTypes.ModuleParts (
        -- * ModuleParts
     ModuleParts(..)
        -- ** Lenses for 'ModuleParts'
    , jsTitle
    , json
    , externalDeps
    , localDeps
    , declaration
        -- ** Accumulating functions
    , appendAofPart
    , appendRecord
    ) where

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
    ModuleParts {
       -- | Optional title. Usually obtained as @obj .:? "title"@
         _jsTitle      :: Maybe String
      -- | Set of all files that have been included via @!include file.yaml@
        , _externalDeps :: Set FilePath
      -- | Map from fieldname to subschema that was described in-place.
        , _localDeps    :: Map TR.ModuleName ModuleParts
      -- | Type structure of current type.
        , _declaration  :: TR.TypeRep
      -- | Original schema
        , _json         :: Text
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON)

makeLenses ''ModuleParts

-- | Function that describes a way to combine types into AllOf or to Anyof type
appendAofPart ::
    (Int, ModuleParts) -- ^ Option number and new option type to add
    -> ModuleParts -- ^ Type to accumulate to
    -> ModuleParts -- ^ Resulting type wich is a combination of two arguments
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

-- | Function that describes a way to combine types into a productType
appendRecord ::
       TR.FieldName -- ^ FieldName to add
    -> (ModuleParts, Bool) -- ^ Field that is added under FieldName and it's required flag
    -> ModuleParts -- ^ Type to add to
    -> ModuleParts -- ^ Resulting type which is a new type with a new field
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
