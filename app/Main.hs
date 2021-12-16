{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards           #-}

module Main where

import qualified Data.Bifunctor
import           Data.Coerce       (coerce)
import           Data.Foldable     (Foldable (foldl'), traverse_)
import           Data.String       (IsString (fromString))

import Control.Monad.Except       (runExcept)
import Control.Monad.Reader       (ReaderT (runReaderT))
import Control.Monad.State.Strict (StateT (runStateT))


import Data.Yaml (FromJSON, decodeHelper)


import qualified Data.Aeson.Key    as K
import qualified Data.Aeson.KeyMap as KM
import           Data.Aeson.Types  (FromJSON (parseJSON))


import System.Directory     (createDirectoryIfMissing)
import System.FilePath      (takeDirectory, (</>))
import System.FilePath.Find (always, extension, find, (==?))

import GHC           (getSessionDynFlags, runGhc)
import GHC.Paths     (libdir)
import GHC.SourceGen (HsModule', showPpr)

import qualified CLI
import           Data.ConfigGen.Parsing  (ParserResult (..))
import           Data.ConfigGen.Traverse (Dep (ToBuild), GeneratorState (GeneratorState),
                                          modulePartsToModules)

import Data.ConfigGen.Parsing.IncludeInjection ( eventsFromFile )

newtype GeneratedModules =
    GeneratedModules
        { unGeneratedModules :: [(String, HsModule')]
        }
    deriving newtype (Semigroup, Monoid)

instance FromJSON GeneratedModules where
    parseJSON v = do
        p@(ParserResult mainType deps) <- parseJSON @ParserResult v
        let q =
                runExcept $
                runStateT (runReaderT (modulePartsToModules mainType) []) $
                GeneratorState . KM.fromList $ Data.Bifunctor.bimap fromString ToBuild <$> deps
        res <-
            case q of
                Left s          -> fail s
                Right (res', _) -> return res'
        return . coerce $ Data.Bifunctor.first K.toString <$> KM.toList res

collectFiles :: FilePath -> IO [FilePath]
collectFiles fp = do 
    res <- find always (extension ==? ".yaml") fp
    print res
    return res

main :: IO ()
main = do
    CLI.CheckedInput {..} <- CLI.getCLIArgs
    files <-
        case chInput of
            CLI.File s -> return [s]
            CLI.Dir s  -> collectFiles s
    content <- sequence <$> traverse (\t -> decodeHelper (eventsFromFile t)) files
    let foldedContent =
            foldl'
                (\acc (_, em) ->
                     case em of
                         Left _                        -> acc
                         Right (m :: GeneratedModules) -> acc <> m)
                (mempty :: GeneratedModules) <$>
            content
    case foldedContent of
        Left pe -> print pe
        Right xs ->
            saveModule $
            GeneratedModules . fmap (Data.Bifunctor.first (chOutput </>)) $
            unGeneratedModules xs
  where
    saveModule :: GeneratedModules -> IO ()
    saveModule gm = traverse_ (uncurry saveFile) $ unGeneratedModules gm
      where
        saveFile :: String -> HsModule' -> IO ()
        saveFile path md = do
            dflags <- runGhc (Just libdir) getSessionDynFlags
            createDirectoryIfMissing True $ takeDirectory path
            writeFile path $ showPpr dflags md

