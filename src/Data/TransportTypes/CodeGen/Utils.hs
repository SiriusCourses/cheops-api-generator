{-|
Module      : Data.TransportTypes.CodeGen.Utils
Description : Helper functions for code generation. 
-}
module Data.TransportTypes.CodeGen.Utils where

import           Data.String                 (fromString)
import           GHC.SourceGen

import qualified Data.TransportTypes.TypeRep as TR

-- | Import declaration to hide Prelude
hidingPrelude :: ImportDecl'
hidingPrelude = flip exposing [] $ import' "Prelude"

-- | Constant for default deriving clauses
defaultDerivingClause :: [HsDerivingClause']
defaultDerivingClause = [generic, schema]
  where
    generic = deriving' [var "GHC.Generics.Generic", var "Prelude.Show", var "Prelude.Eq"]
    schema = derivingAnyclass [var "Data.Swagger.ToSchema"]

-- | Constant for default deriving clauses
minDerivingClause :: [HsDerivingClause']
minDerivingClause = [generic]
  where
    generic = deriving' [var "GHC.Generics.Generic", var "Prelude.Show", var "Prelude.Eq"]

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
