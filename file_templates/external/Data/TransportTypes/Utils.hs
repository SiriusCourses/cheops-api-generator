module Data.TransportTypes.Utils where

import Data.Proxy (Proxy)

type UnitProxy = Proxy ()

class IsEmpty a where
    isEmpty :: a -> Bool

emptynessCheck :: (IsEmpty a) => a -> Maybe a
emptynessCheck obj =
    if isEmpty obj
        then Nothing
        else Just obj
