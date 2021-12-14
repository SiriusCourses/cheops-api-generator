module Ratio where
import qualified Ratio.Notlala
import qualified GHC.Types
import qualified GHC.Int
import qualified Data.Text
import qualified Data.Vector
data Ratio
  = Ratio {denum :: !Int,
           lala :: !Ratio.Notlala.Notlala,
           num :: !Int}