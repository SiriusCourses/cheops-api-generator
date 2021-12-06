{-# LANGUAGE PatternSynonyms #-}

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

type ModuleName = String

data TypeRef
    = ExtRef NonLocalRef
    | ReferenceToLocalType ModuleName
    deriving (Show, Eq)

pattern ReferenceToExternalType :: ModuleName -> TypeRef

pattern ReferenceToExternalType mn = ExtRef (RefExternalType mn)

pattern ReferenceToPrimitiveType :: String -> TypeRef

pattern ReferenceToPrimitiveType s = ExtRef (RefPrimitiveType s)

{-# COMPLETE ReferenceToLocalType, ReferenceToExternalType,
  ReferenceToPrimitiveType #-}

data NonLocalRef
    = RefPrimitiveType String
    | RefExternalType ModuleName
    deriving (Show, Eq)

data TypeRep
    = ProdType (KeyMap TypeRef)
    | SumType (KeyMap TypeRef)
    | NewType String NonLocalRef
    -- Here will go allOf, anyOf, oneOf
    | Ref NonLocalRef
    deriving (Show, Eq)

appendToTypeRep :: TypeRep -> Key -> TypeRef -> TypeRep
appendToTypeRep typeRep k tr =
    case typeRep of
        ProdType km -> ProdType $ KM.insert k tr km
        SumType km  -> SumType $ KM.insert k tr km
        nt@(NewType _ _) -> nt
        Ref tr'     -> Ref tr'

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
    deriving (Show, Eq)

getReference :: ModuleParts -> Maybe ModuleName
getReference ModuleParts {_declaration = (Ref (RefExternalType s))} = Just s
getReference _                                                      = Nothing
