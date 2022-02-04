{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.TransportTypes.TypeRep
    ( TypeRef(.., ReferenceToExternalType, ReferenceToPrimitiveType,
        ReferenceToLocalType)
    , TypeRep(..)
    , NonLocalRef(..)
    , LocalReference(..)
    , ModuleName
    , FieldName
    , TypeName
    , Field(..)
    , SumConstr
    , SumConstrF(..)
    ) where

import GHC.Generics (Generic)

import Data.Yaml (ToJSON, Value)

import Data.Map (Map)

type ModuleName = String

type TypeName = String

type FieldName = String

data TypeRef
    = ExtRef NonLocalRef
    | LocRef LocalReference
    deriving (Show, Eq, Generic, Ord)
    deriving anyclass (ToJSON)

data LocalReference =
    LocalReference FieldName TypeName
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON)

data NonLocalRef
    = RefPrimitiveType String
    | RefExternalType ModuleName TypeName
    deriving (Show, Eq, Generic, Ord)
    deriving anyclass (ToJSON)

data Field =
    Field Bool TypeRef
    deriving (Show, Eq, Generic, Ord)
    deriving (ToJSON)

type SumConstr = SumConstrF Field

newtype SumConstrF a =
    SumConstr
        { unSumConstr :: [a]
        }
    deriving (Show, Eq, Generic)
    deriving newtype (Functor, Applicative, Monad, Ord)
    deriving anyclass (ToJSON)

data TypeRep
    = ProdType (Map FieldName Field) Bool
    | SumType [(FieldName, SumConstr)]
    | OneOfType [(FieldName, SumConstr)]
    | AnyOfType [TypeRef]
    | AllOfType [TypeRef]
    | ArrayType TypeRef
    | NewType TypeRef
    | Ref NonLocalRef
    | ConstType Value
    deriving (Show, Eq, Generic, Ord)
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
