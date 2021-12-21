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
    , appendToTypeRep
    , isLocal
    , getReference
    , isExtDep
    , toString
    , moduleNmToQualTypeName
    ) where

import           Control.Lens      (makeLenses)
import           Data.Set          (Set)
import           GHC.Generics      (Generic)

import Data.Yaml (ToJSON)
import Util      (capitalise, split)

import Data.Map (Map)
import qualified Data.Map as Map

type ModuleName = String
type FieldName = String

data TypeRef
    = ExtRef NonLocalRef
    | LocRef LocalReference
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON)

newtype LocalReference =
    LocalReference
        { unLocalRef :: ModuleName
        }
    deriving (Generic)
    deriving newtype (Show, Eq)
    deriving anyclass (ToJSON)

pattern ReferenceToLocalType :: ModuleName -> TypeRef

pattern ReferenceToLocalType nm = LocRef (LocalReference nm)

pattern ReferenceToExternalType :: ModuleName -> TypeRef

pattern ReferenceToExternalType mn = ExtRef (RefExternalType mn)

pattern ReferenceToPrimitiveType :: String -> TypeRef

pattern ReferenceToPrimitiveType s = ExtRef (RefPrimitiveType s)

{-# COMPLETE ReferenceToLocalType, ReferenceToExternalType,
  ReferenceToPrimitiveType #-}

toString :: TypeRef -> String
toString (ReferenceToLocalType s)     = s
toString (ReferenceToExternalType s)  = s
toString (ReferenceToPrimitiveType s) = s

data NonLocalRef
    = RefPrimitiveType String
    | RefExternalType ModuleName
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON)

data TypeRep
    = ProdType (Map FieldName TypeRef)
    | SumType (Map FieldName TypeRef)
    | ArrayType TypeRef
    | NewType String TypeRef
    -- Here will go allOf, anyOf, oneOf
    | Ref NonLocalRef
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON)

appendToTypeRep :: TypeRep -> FieldName -> TypeRef -> TypeRep
appendToTypeRep typeRep k tr =
    case typeRep of
        ProdType km       -> ProdType $ Map.insert k tr km
        SumType km        -> SumType $ Map.insert k tr km
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
        , _externalDeps :: Set FilePath
        , _localDeps    :: Map ModuleName ModuleParts
        , _declaration  :: TypeRep
        }
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON)

makeLenses ''ModuleParts

getReference :: ModuleParts -> Maybe ModuleName
getReference ModuleParts {_declaration = (Ref (RefExternalType s))} = Just s
getReference _                                                      = Nothing

moduleNmToQualTypeName :: ModuleName -> String
moduleNmToQualTypeName mn = mn ++ "." ++ (capitalise . last $ split '.' mn)

