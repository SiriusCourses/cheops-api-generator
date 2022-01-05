{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TransportTypes.Deriv where

import Data.Yaml (Object, ToJSON (..), Value (..))

import           GHC.Generics
import qualified GHC.Generics as G

newtype AOf a =
    AOf a

class GToJSON f where
    gToJSON :: f a -> Object

instance (Generic a, GToJSON (Rep a)) => ToJSON (AOf a) where
    toJSON (AOf x) = Object $ gToJSON (from x)

instance GToJSON G.U1 where
    gToJSON _ = mempty

instance ToJSON c => GToJSON (G.K1 i c) where
    gToJSON (G.K1 x') =
        case toJSON x' of
            Object x -> x
            _        -> mempty

instance GToJSON f => GToJSON (G.M1 i c f) where
    gToJSON (G.M1 x) = gToJSON x

instance (GToJSON a, GToJSON b) => GToJSON (a :*: b) where
    gToJSON (a :*: b) = gToJSON a <> gToJSON b

instance (GToJSON a, GToJSON b) => GToJSON (a :+: b) where
    gToJSON (L1 x) = gToJSON x
    gToJSON (R1 x) = gToJSON x
