module Data.TransportTypes.ArbitraryInstanceForValue where

import Data.Aeson
import Test.QuickCheck
import Test.QuickCheck.Instances

import qualified Data.HashMap.Strict as HMap
import qualified Data.Vector         as V

instance Arbitrary Value where
    arbitrary =
        oneof
            [ Number <$> arbitrary
            , String <$> arbitrary
            , Array . V.fromList <$> resize 2 arbitrary
            , Object . HMap.fromList <$> resize 2 arbitrary
            , Bool <$> arbitrary
            ]
