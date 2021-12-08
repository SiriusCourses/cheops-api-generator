{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.ConfigGen.TypeRep
    ( TypeRef(.., ReferenceToExternalType, ReferenceToPrimitiveType)
    , TypeRep(..)
    , ModuleParts(..)
    , NonLocalRef(..)
    , ModuleName
    , appendToTypeRep
    , isLocal
    , getReference
    , isExtDep
    ) where

import           Data.Aeson.Key    (Key)
import           Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import           Data.Set          (Set)
import           GHC.Generics      (Generic)

import Data.Yaml (ToJSON)

type ModuleName = String


data TypeRef
    = ExtRef NonLocalRef
    | ReferenceToLocalType ModuleName
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON)

pattern ReferenceToExternalType :: ModuleName -> TypeRef

pattern ReferenceToExternalType mn = ExtRef (RefExternalType mn)

pattern ReferenceToPrimitiveType :: String -> TypeRef

pattern ReferenceToPrimitiveType s = ExtRef (RefPrimitiveType s)

{-# COMPLETE ReferenceToLocalType, ReferenceToExternalType,
  ReferenceToPrimitiveType #-}

data NonLocalRef
    = RefPrimitiveType String
    | RefExternalType ModuleName
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON)

data TypeRep
    = ProdType (KeyMap TypeRef)
    | SumType (KeyMap TypeRef)
    | ArrayType TypeRef
    | NewType String TypeRep
    -- Here will go allOf, anyOf, oneOf
    | Ref NonLocalRef
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON)

appendToTypeRep :: TypeRep -> Key -> TypeRef -> TypeRep
appendToTypeRep typeRep k tr =
    case typeRep of
        ProdType km       -> ProdType $ KM.insert k tr km
        SumType km        -> SumType $ KM.insert k tr km
        nt@(NewType _ _)  -> nt
        arr@(ArrayType _) -> arr
        Ref tr'           -> Ref tr'

isLocal :: TypeRep -> Bool
isLocal (ProdType _) = True
isLocal (SumType _)  = True
isLocal _            = False

isExtDep :: TypeRep -> Bool
isExtDep (Ref (RefExternalType _)) = True
isExtDep _                         = False

data ModuleParts =
    ModuleParts
        { _jsTitle      :: Maybe String
        , _externalDeps :: Set ModuleName
        , _localDeps    :: KeyMap ModuleParts
        , _declaration  :: TypeRep
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON)

getReference :: ModuleParts -> Maybe ModuleName
getReference ModuleParts {_declaration = (Ref (RefExternalType s))} = Just s
getReference _                                                      = Nothing