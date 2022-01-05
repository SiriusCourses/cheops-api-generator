module Data.TransportTypes.CodeGen.Hylo.Structure where

import Data.Map.Strict (Map)
import Data.Set        (Set)

import           Data.TransportTypes.CodeGen.NamingUtils (ModuleName)
import qualified Data.TransportTypes.CodeGen.NamingUtils as U
import           Data.TransportTypes.TypeRep             (TypeRep)

data NodeF a s
    = Local a (Map ModuleName s)
    | Leaf a

getPayload :: NodeF a s -> a
getPayload (Local p _) = p
getPayload (Leaf p)    = p

data Payload =
    Payload
        { title        :: Maybe U.Title
        , externalDeps :: Set FilePath
        , typeRep      :: TypeRep
        }

instance Functor (NodeF a) where
    fmap _ (Leaf x)       = Leaf x
    fmap fab (Local x km) = Local x $ fmap fab km
