{-# LANGUAGE PatternSynonyms #-}

module Data.ConfigGen.Types.InternalRep where

import qualified Data.Text as T
import qualified Bound as B

type ConstructorName = String

type FieldName = String

data BasicRepType
    = StringLit
    | Sci Scientific
    | TextRep T.Text

data Scientific
    = Int32
    | Int64
    | Double
    | Float

pattern Number :: BasicRepType

pattern Number = Sci Int32

data InternalRepType a
    = Basic BasicRepType
    | Sum [(ConstructorName, InternalRepType a)]
    | Product [(FieldName, InternalRepType a)]
    | Array (InternalRepType a)
    | App (InternalRepType a) (InternalRepType a)
    | Lam (B.Scope () InternalRepType a)
    | Var a
