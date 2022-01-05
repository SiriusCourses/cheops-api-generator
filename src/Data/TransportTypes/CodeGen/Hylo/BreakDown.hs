module Data.TransportTypes.CodeGen.Hylo.BreakDown where

import           Data.TransportTypes.CodeGen.Hylo.Internal  (Coalgebra)
import           Data.TransportTypes.CodeGen.Hylo.Structure (NodeF (..), Payload (..))
import qualified Data.TransportTypes.ModuleParts            as MP

breakDown :: Coalgebra (NodeF Payload) MP.ModuleParts
breakDown (MP.ModuleParts _jsTitle _externalDeps _localDeps _declaration)
    | null _localDeps = Leaf payload
    | otherwise = Local payload _localDeps
  where
    payload = Payload _jsTitle _externalDeps _declaration
