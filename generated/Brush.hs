module Brush where
import qualified GHC.Types
import qualified GHC.Int
import qualified Data.Text
import qualified Data.Vector
newtype Brush = Brush {unBrush :: Data.Text.Text}