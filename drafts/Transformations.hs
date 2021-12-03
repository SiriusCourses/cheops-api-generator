module Data.ConfigGen.Types.Transformations where

import qualified Data.ConfigGen.Types.BackendRep  as B
import qualified Data.ConfigGen.Types.FrontendRep as F
import qualified Data.ConfigGen.Types.InternalRep as I

import GHC.SourceGen (HsType')

toHsType :: I.InternalRepType a -> HsType'
toHsType = undefined
  where
    transformBasicTypes :: I.BasicRepType -> HsType'
    transformBasicTypes = undefined

toFrontendType :: I.InternalRepType a -> F.FrontendRep a
toFrontendType = undefined
  where
    transformBasicTypes :: I.BasicRepType -> F.FrontendRep a -- secretly is a toJSON instance
    transformBasicTypes = undefined

fromBackendType :: B.BackendRep -> I.InternalRepType a
fromBackendType = undefined
