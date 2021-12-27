{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.ConfigGen.TypeRep
    ( TypeRef(.., ReferenceToExternalType, ReferenceToPrimitiveType,
        ReferenceToLocalType)
    , TypeRep(..)
    , ModuleParts(..)
    , declaration
    , externalDeps
    , localDeps
    , jsTitle
    , NonLocalRef(..)
    , LocalReference(..)
    , ModuleName
    , FieldName
    , TypeName
    , appendToTypeRep
    , Field(..)
    , SumConstr
    , SumConstrF(..)
    ) where

import Control.Lens (makeLenses)
import Data.Set     (Set)
import GHC.Generics (Generic)

import Data.Yaml (ToJSON)

import           Data.Map (Map)
import qualified Data.Map as Map

type ModuleName = String

type TypeName = String

type FieldName = String

data TypeRef
    = ExtRef NonLocalRef
    | LocRef LocalReference
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON)

data LocalReference =
    LocalReference FieldName TypeName
    deriving (Generic)
    deriving (Show, Eq, Ord)
    deriving anyclass (ToJSON)

data NonLocalRef
    = RefPrimitiveType String
    | RefExternalType ModuleName TypeName
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON)

data Field =
    Field Bool TypeRef
    deriving (Show, Eq, Generic)
    deriving (ToJSON)

type SumConstr = SumConstrF Field

newtype SumConstrF a =
    SumConstr
        { unSumConstr :: [a]
        }
    deriving (Show, Eq, Generic)
    deriving newtype (Functor, Applicative, Monad)
    deriving anyclass (ToJSON)

data TypeRep
    = ProdType (Map FieldName Field)
    | SumType (Map FieldName SumConstr)
    | AnyOf 
    | OneOf 
    | AllOf 
    | ArrayType TypeRef
    | NewType TypeRef
    | Ref NonLocalRef
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON)

pattern ReferenceToLocalType :: FieldName -> TypeName -> TypeRef

pattern ReferenceToLocalType nm tn = LocRef (LocalReference nm tn)

pattern ReferenceToExternalType ::
        ModuleName -> TypeName -> TypeRef

pattern ReferenceToExternalType mn tn =
        ExtRef (RefExternalType mn tn)

pattern ReferenceToPrimitiveType :: String -> TypeRef

pattern ReferenceToPrimitiveType s = ExtRef (RefPrimitiveType s)

{-# COMPLETE ReferenceToLocalType, ReferenceToExternalType,
  ReferenceToPrimitiveType #-}

appendToTypeRep :: TypeRep -> FieldName -> Field -> TypeRep
appendToTypeRep typeRep k tr =
    case typeRep of
        ProdType km       -> ProdType $ Map.insert k tr km
        SumType km        -> SumType $ Map.insert k (SumConstr [tr]) km
        nt@(NewType _)    -> nt
        arr@(ArrayType _) -> arr
        _x                -> _x

data ModuleParts =
    ModuleParts
        { _jsTitle      :: Maybe String
        , _externalDeps :: Set FilePath
        , _localDeps    :: Map ModuleName ModuleParts
        , _declaration  :: TypeRep
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON)

makeLenses ''ModuleParts
