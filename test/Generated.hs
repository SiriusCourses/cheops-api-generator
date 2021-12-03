module Generated where
import qualified GHC.Types
import qualified GHC.Int
import qualified Data.Text
data Lala = Lala {lenght :: !GHC.Int.Int64, size :: !GHC.Types.Int}
data Glossary
  = Glossary {name :: !Data.Text.Text,
              nested_ratio :: !Ratio,
              size' :: !GHC.Types.Int,
              somthingelse :: !GHC.Types.Int}
data LatexRequest
  = LatexRequest {glossary :: !Glossary, ratio :: !Ratio}
data Ratio
  = Ratio {denum :: !GHC.Types.Int,
           lala :: !Lala,
           num :: !GHC.Int.Int64}
