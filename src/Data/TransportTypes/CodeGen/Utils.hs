module Data.TransportTypes.CodeGen.Utils where

import           Data.String                 (fromString)
import           GHC.SourceGen

import qualified Data.TransportTypes.TypeRep as TR

-- | Import declaration to hide Prelude
hidingPrelude :: ImportDecl'
hidingPrelude = flip exposing [] $ import' "Prelude"

-- | Constant for default deriving clauses
defaultDerivingClause :: [HsDerivingClause']
defaultDerivingClause = [generic, json]
  where
    generic = deriving' [var "GHC.Generics.Generic", var "Prelude.Show", var "Prelude.Eq"]
    json = derivingAnyclass [var "Data.Yaml.ToJSON", var "Data.Yaml.FromJSON"]

-- | Constant for default deriving clauses
minDerivingClause :: [HsDerivingClause']
minDerivingClause = [generic, json]
  where
    generic = deriving' [var "GHC.Generics.Generic", var "Prelude.Show", var "Prelude.Eq"]
    json = derivingAnyclass [var "Data.Yaml.FromJSON"]

-- | Takes type name and produces deriving clauses for "allOf", "anyOf" and "oneOf" for said type
aofDerivingClause :: TR.TypeName -> [HsDerivingClause']
aofDerivingClause typename = [tojson, fromjson, generic]
  where
    generic = deriving' [var "GHC.Generics.Generic", var "Prelude.Show", var "Prelude.Eq"]
    fromjson = derivingAnyclass [var "Data.Yaml.FromJSON"]
    tojson =
        derivingVia
            (var "Data.TransportTypes.Deriv.AOf" @@ (var . fromString $ typename))
            [var "Data.Yaml.ToJSON"] --, var "Data.Yaml.FromJSON"]
