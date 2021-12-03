module Data.ConfigGen.Types.FrontendRep where

import Data.ConfigGen.Types.InternalRep (InternalRepType)

newtype FrontendRep a =
    FrontendRep
        { unFrontendRep :: InternalRepType a
        }
