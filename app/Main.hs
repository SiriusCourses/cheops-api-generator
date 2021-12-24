{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards           #-}

module Main where

import qualified Data.Bifunctor
import           Data.Foldable  (Foldable (foldl'), traverse_)
import qualified Data.Map       as Map

import Data.Yaml (decodeHelper)

import System.Directory     (canonicalizePath, createDirectoryIfMissing)
import System.FilePath      (takeDirectory, (</>))
import System.FilePath.Find (always, extension, find, (==?))

import GHC           (getSessionDynFlags, runGhc)
import GHC.Paths     (libdir)
import GHC.SourceGen (HsModule', showPpr)

import qualified CLI
import           Data.ConfigGen.Parsing  (ParserResult (..), postprocessParserResult,
                                          replaceDashesWithUnderscores)
import           Data.ConfigGen.Traverse (build)

import           Control.Monad                           (unless, when)
import           Data.ConfigGen.Parsing.IncludeInjection (eventsFromFile)
import           Data.ConfigGen.TypeRep                  (ModuleParts (ModuleParts))
import qualified Data.ConfigGen.TypeRep                  as TR
import           Data.Either                             (fromRight, isLeft, isRight)

newtype GeneratedModules =
    GeneratedModules
        { unGeneratedModules :: [(String, HsModule')]
        }
    deriving newtype (Semigroup, Monoid)

collectFiles :: FilePath -> IO [FilePath]
collectFiles path = do
    files <- find always (extension ==? ".yaml") path
    traverse canonicalizePath files

main :: IO ()
main = do
    CLI.CheckedInput {..} <- CLI.getCLIArgs
    files <-
        case chInput of
            CLI.File s -> return [s]
            CLI.Dir s  -> collectFiles s
    -- traverse_ putStrLn files
    content <- sequence <$> traverse (decodeHelper @ParserResult . eventsFromFile) files
    case content of
        Left pe -> print pe
        Right x0 -> do
            res <- extractParsedModules x0 files
            when chDebug $ putStrLn "-- resulting parser output:"
            when chDebug $ print res
            when (null res) $
                fail
                    ("No files are successfully parsed. Content looks like this: " ++
                     show content)
            let ini =
                    ParserResult
                        (ModuleParts
                             (Just "dummy")
                             mempty
                             mempty
                             (TR.Ref . TR.RefPrimitiveType $ "Int"))
                        mempty
            let acc =
                    replaceDashesWithUnderscores . postprocessParserResult $
                    foldl' combineParserResults ini res
            when chDebug $ putStrLn "-- accumulated parser results"
            when chDebug $ print acc
            let b = build acc
            when chDebug $ putStrLn "-- Built Modules:"
            when chDebug $ print . Map.keys $ fromRight mempty b
            case b of
                Left s   -> fail s
                Right km -> saveModule $ Data.Bifunctor.first (chOutput </>) <$> Map.toList km
  where
    combineParserResults :: ParserResult -> (FilePath, ParserResult) -> ParserResult
    combineParserResults (ParserResult mainType deps) (p, ParserResult mainType' deps') =
        ParserResult mainType newDeps
      where
        newDeps = fromKM . Map.insert p mainType' $ toKM deps <> toKM deps'
        toKM = Map.fromList
        fromKM = Map.toList
    extractParsedModules ::
           Show a
        => [([a], Either String ParserResult)]
        -> [FilePath]
        -> IO [(FilePath, ParserResult)]
    extractParsedModules x0 paths = do
        traverse_
            (\x -> do
                 unless (null . fst $ x) (print . fst $ x)
                 when (isLeft . snd $ x) $ print x)
            x0
        let res =
                map (\(p, Right r) -> (p, r)) .
                filter (\(_, q) -> isRight q) . zip paths . map snd $
                x0
        return res
    saveModule :: [(FilePath, HsModule')] -> IO ()
    saveModule km = traverse_ (uncurry saveFile) km
      where
        saveFile :: String -> HsModule' -> IO ()
        saveFile path md = do
            dflags <- runGhc (Just libdir) getSessionDynFlags
            createDirectoryIfMissing True $ takeDirectory path
            writeFile path $ showPpr dflags md
