module Data.TransportTypes.Utils where

import qualified Data.Aeson.KeyMap as KM
import           Data.Map          ((!?))
import qualified Data.Map          as Map
import           Data.Maybe        (isNothing)
import           Data.Proxy        (Proxy)
import           Data.Yaml         (Value (..), object)

type UnitProxy = Proxy ()

addEmptyPropertiesToObjects :: Value -> Value
addEmptyPropertiesToObjects (Object km)
    | Just t <- map' !? "type" =
        let isObj = t == "object"
            props = map' !? "properties"
            specials = any (`Map.member` map') ["anyOf", "oneOf", "allOf", "const", "enum"]
         in Object $
            fmap addEmptyPropertiesToObjects . KM.fromMap $
            if isObj && isNothing props && not specials
                then Map.insert "properties" (object []) map'
                else map'
    | otherwise = Object $ fmap addEmptyPropertiesToObjects . KM.fromMap $ map'
  where
    map' = KM.toMap km
addEmptyPropertiesToObjects (Array vec) = Array $ fmap addEmptyPropertiesToObjects vec
addEmptyPropertiesToObjects x = x
