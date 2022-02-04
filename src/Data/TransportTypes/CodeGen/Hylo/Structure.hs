{-|
Module      : Data.TransportTypes.CodeGen.Hylo.Structure

Splitting tree structure and its traversal from the data inside nodes.
-}

module Data.TransportTypes.CodeGen.Hylo.Structure
    ( NodeF(..)
    , Payload(..)
    , getPayload
    ) where

import Data.Map.Strict (Map)
import Data.Set        (Set)

import           Data.Text                               (Text)
import           Data.TransportTypes.CodeGen.NamingUtils (ModuleName)
import qualified Data.TransportTypes.CodeGen.NamingUtils as U
import           Data.TransportTypes.TypeRep             (TypeRep)

-- | Tree structure for hylomorphism-like traverse
data NodeF a s
    = Local a (Map ModuleName s)
    | Leaf a

instance Functor (NodeF a) where
    fmap _ (Leaf x)       = Leaf x
    fmap fab (Local x km) = Local x $ fmap fab km

-- | Convinience function for getting payload from 'NodeF'
getPayload :: NodeF a s -> a
getPayload (Local p _) = p
getPayload (Leaf p)    = p

-- | Type which represents 'Data.TransportTypes.ModuleParts.ModuleParts' without tree structure
data Payload =
    Payload
        { title        :: Maybe U.Title
        , externalDeps :: Set FilePath
        , typeRep      :: TypeRep
        , json         :: Text
        }
