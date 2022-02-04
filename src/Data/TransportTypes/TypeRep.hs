{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE PatternSynonyms #-}

{-|
Module      : Data.TransportTypes.TypeRep

This module describes type wich stores type structure of parsed json value.
-}
module Data.TransportTypes.TypeRep
    (
    -- * Type inforamtion of parsed json value.
    TypeRep(..)
    , Field(..)
    , SumConstr
    , SumConstrF(..)
    -- * Reference to subschems
    , TypeRef(.., ReferenceToExternalType, ReferenceToPrimitiveType,
        ReferenceToLocalType)
    -- ** Two different types of refrences
    , NonLocalRef(..)
    , LocalReference(..)
    -- * Semantic type aliases for 'String'
    , ModuleName
    , FieldName
    , TypeName
    ) where

import GHC.Generics (Generic)

import Data.Yaml (ToJSON, Value)

import Data.Map (Map)

type ModuleName = String

type TypeName = String

type FieldName = String

-- | Subschema reference
data TypeRef
        -- | Reference to subschema that is not defined in-place
    = ExtRef NonLocalRef
        -- | Reference to subschema that defined in-place
    | LocRef LocalReference
    deriving (Show, Eq, Generic, Ord)
    deriving anyclass (ToJSON)

-- | Reference to in-place subschema
data LocalReference =
    LocalReference FieldName TypeName
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON)

-- | Reference to out-of-place subschema
data NonLocalRef
      -- | Reference to type defined out of yaml specification. Contains fully qualified type name.
    = RefPrimitiveType String
      -- | Reference to type defined out of file, but in yaml current yaml specification. Containes qualifed module name and not qualified typename
    | RefExternalType ModuleName TypeName
    deriving (Show, Eq, Generic, Ord)
    deriving anyclass (ToJSON)

-- | Reference to a type from product type field
data Field =
    -- | Contains requiredment flag and a reference itself
    Field Bool TypeRef

    deriving (Show, Eq, Generic, Ord)
    deriving (ToJSON)

-- | Reference to a type from sum type contructor
type SumConstr = SumConstrF Field


newtype SumConstrF a =
    SumConstr
        { unSumConstr :: [a]
        }
    deriving (Show, Eq, Generic)
    deriving newtype (Functor, Applicative, Monad, Ord)
    deriving anyclass (ToJSON)

data TypeRep
    -- | Product type with fields and additional properties flag
    = ProdType (Map FieldName Field) Bool
    -- | Sum type with constructors. Secretly can be only enum type
    | SumType [(FieldName, SumConstr)]
    -- | OneOf type with its alternatives
    | OneOfType [(FieldName, SumConstr)]
    -- | AnyOf type with its alternatives
    | AnyOfType [TypeRef]
    -- | AnyOf type with all its subtypes
    | AllOfType [TypeRef]
    -- | Array type with its item type
    | ArrayType TypeRef
    -- | Newtype with its wrapped type
    | NewType TypeRef
    -- | Also newtype but it refrences only out-of-place types
    | Ref NonLocalRef
    -- | COnst type with its value. Good behaviour only on strings.
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
