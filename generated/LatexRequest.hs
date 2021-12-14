module LatexRequest where
import qualified Ratio
import qualified LatexRequest.Glossary
import qualified GHC.Types
import qualified GHC.Int
import qualified Data.Text
import qualified Data.Vector
data LatexRequest
  = LatexRequest {glossary :: !LatexRequest.Glossary.Glossary,
                  ratio :: !Ratio.Ratio}