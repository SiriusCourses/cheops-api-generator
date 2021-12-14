module LatexRequest.Glossary where
import qualified Ratio
import qualified LatexRequest.Glossary.Content
import qualified LatexRequest.Glossary.Somethingelse
import qualified GHC.Types
import qualified GHC.Int
import qualified Data.Text
import qualified Data.Vector
data Glossary
  = Glossary {content :: !LatexRequest.Glossary.Content.Content,
              name :: !Data.Text.Text,
              nested_ratio :: !Ratio.Ratio,
              size :: !Int,
              somethingelse :: !LatexRequest.Glossary.Somethingelse.Somethingelse}