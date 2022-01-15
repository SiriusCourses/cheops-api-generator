module Data.TransportTypes.CodeGen
    ( buildTests
    , buildModules
    ) where


import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import GHC.SourceGen ( HsModule' )

import           Data.TransportTypes.CodeGen.Hylo        (build)
import qualified Data.TransportTypes.CodeGen.NamingUtils as U
import           Data.TransportTypes.Parsing             (ParserResult (..))
import Data.TransportTypes.CodeGen.TypeGen (buildModule)
import Data.TransportTypes.CodeGen.TestGen (buildTest, buildSpec)


buildModules :: ParserResult -> Either String (Map FilePath HsModule')
buildModules = build buildModule

buildTests :: ParserResult -> Either String (Map FilePath HsModule')
buildTests pr = do
    t <- Map.mapKeys U.testFilePathFromModuleFilePath <$> build buildTest pr
    return $ Map.insert U.specPath (buildSpec $ Map.keys t) t

