{-|
Module      : Data.TransportTypes.CodeGen.Hylo

Reexports 'build' function and tree node 'Payload'
-}

module Data.TransportTypes.CodeGen.Hylo
    ( build
    , Payload(..)
    ) where

import Data.TransportTypes.CodeGen.Hylo.BuildUp   (build)
import Data.TransportTypes.CodeGen.Hylo.Structure (Payload (..))
