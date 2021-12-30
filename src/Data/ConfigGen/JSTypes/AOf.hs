{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneKindSignatures  #-}

module Data.ConfigGen.JSTypes.AOf where

import Data.Kind
import Data.Yaml (FromJSON (..), ToJSON (..), Value (..))

type AllOf :: [Type] -> Type
newtype AllOf u = AllOf Value

instance FromJSON (AllOf l) where
    parseJSON x = return $ AllOf x

instance ToJSON (AllOf l) where
    toJSON (AllOf x) = x


type AnyOf :: [Type] -> Type
newtype AnyOf u = AnyOf Value

instance FromJSON (AnyOf l) where
    parseJSON x = return $ AnyOf x

instance ToJSON (AnyOf l) where
    toJSON (AnyOf x) = x