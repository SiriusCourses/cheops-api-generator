{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Data.TransportTypes.CodeGen.Hylo.BreakDown

Defines 'Coalgbra' for hylomorhism
-}

module Data.TransportTypes.CodeGen.Hylo.BreakDown where

import           Data.TransportTypes.CodeGen.Hylo.Internal  (Coalgebra)
import           Data.TransportTypes.CodeGen.Hylo.Structure (NodeF (..), Payload (..))
import qualified Data.TransportTypes.ModuleParts            as MP

-- | Converts tree with representd with 'MP.ModuleParts' to tree represented with 'NodeF Payload'
breakDown :: Coalgebra (NodeF Payload) MP.ModuleParts
breakDown MP.ModuleParts {..}
    | null _localDeps = Leaf payload
    | otherwise = Local payload _localDeps
  where
    payload = Payload _jsTitle _externalDeps _declaration _json
