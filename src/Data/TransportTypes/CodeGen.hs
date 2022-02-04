{-|
Module      : Data.TransportTypes.CodeGen

Interface for code generation. Allows to build types and tests for them. 
-}

module Data.TransportTypes.CodeGen
        
    ( 
    -- * Tools for code generation
      buildTests
    , buildModules
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import GHC.SourceGen (HsModule')

import           Data.TransportTypes.CodeGen.Hylo        (build)
import qualified Data.TransportTypes.CodeGen.NamingUtils as U
import           Data.TransportTypes.CodeGen.TestGen     (buildSpec, buildTest)
import           Data.TransportTypes.CodeGen.TypeGen     (buildModule)
import           Data.TransportTypes.Parsing             (ParserResult (..))

-- | Takes 'ParserResult' and produces types with their paths or fails
buildModules :: ParserResult -> Either String (Map FilePath HsModule')
buildModules = build buildModule

-- | Takes 'ParserResult' and produces tests with their paths or fails
buildTests :: ParserResult -> Either String (Map FilePath HsModule')
buildTests pr = do
    t <- Map.mapKeys U.testFilePathFromModuleFilePath <$> build buildTest pr
    return $ Map.insert U.specPath (buildSpec $ Map.keys t) t
