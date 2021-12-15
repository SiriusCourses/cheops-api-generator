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
    , appendToTypeRep
    , isLocal
    , getReference
    , isExtDep
    , getNameFromReference
    , importToExportedType
    ) where

import           Control.Lens      (makeLenses)
import           Data.Aeson.Key    (Key)
import           Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import           Data.Set          (Set)
import           GHC.Generics      (Generic)

import Data.Yaml (ToJSON)
import Util      (capitalise, split)

type ModuleName = String

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

getNameFromReference :: TypeRef -> String
getNameFromReference (ReferenceToLocalType s)     = s
getNameFromReference (ReferenceToExternalType s)  = s
getNameFromReference (ReferenceToPrimitiveType s) = s

data NonLocalRef
    = RefPrimitiveType String
    | RefExternalType ModuleName
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON)

data TypeRep
    = ProdType (KeyMap TypeRef)
    | SumType (KeyMap TypeRef)
    | ArrayType TypeRef
    | NewType String TypeRef
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

makeLenses ''ModuleParts

getReference :: ModuleParts -> Maybe ModuleName
getReference ModuleParts {_declaration = (Ref (RefExternalType s))} = Just s
getReference _                                                      = Nothing

importToExportedType :: ModuleName -> String
importToExportedType mn = mn ++ "." ++ (capitalise . last $ split '.' mn)
