{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards           #-}

module Main where

import           Control.Monad  (unless, when)
import qualified Data.Bifunctor
import           Data.Foldable  (Foldable (foldl'), for_, traverse_)
import qualified Data.Map       as Map

import Data.Yaml (decodeHelper)

import System.Directory     (canonicalizePath, createDirectoryIfMissing)
import System.FilePath      (takeDirectory, (</>))
import System.FilePath.Find (always, extension, find, (==?))

import GHC           (getSessionDynFlags, runGhc)
import GHC.Paths     (libdir)
import GHC.SourceGen (HsModule', showPpr)

import qualified CLI
import           Data.ConfigGen.Parsing  (ParserResult (..), dashesToUnderscore,
                                          postprocessParserResult, transformStrings)
import           Data.ConfigGen.Traverse (build)

import           Conduit                                 (liftIO)
import           Data.ConfigGen.ModuleParts              (ModuleParts (..))
import           Data.ConfigGen.Parsing.IncludeInjection (RepositoryRoot (..), eventsFromFile)
import qualified Data.ConfigGen.TypeRep                  as TR
import           Data.Either                             (fromRight, isLeft, isRight, lefts)
import           Data.Maybe                              (fromJust, isJust)
import qualified System.ProgressBar                      as PB
import           Text.Casing                             (camel)
import           Util                                    (singleton)

collectFiles :: FilePath -> IO [FilePath]
collectFiles path = do
    files <- find always (extension ==? ".yaml") path
    traverse canonicalizePath files

main :: IO ()
main = do
    CLI.CheckedInput {..} <- CLI.getCLIArgs
    files <-
        case chInput of
            CLI.File s -> singleton <$> canonicalizePath s
            CLI.Dir s  -> collectFiles s
    crr <- canonicalizePath chRoot
    content <-
        do 
            putStrLn "Parsing files:"
            pb <- PB.newProgressBar PB.defStyle 10 (PB.Progress 0 (length files) ())
            sequence <$>
                traverse
                    (\f -> do
                            res <-
                                decodeHelper @ParserResult . eventsFromFile (RepositoryRoot crr) $
                                f
                            liftIO $ PB.incProgress pb 1
                            return res)
                    files
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
                    transformStrings (camel . dashesToUnderscore) . postprocessParserResult $
                    foldl' combineParserResults ini res
            when chDebug $ putStrLn "-- accumulated files"
            when chDebug $ traverse_ (print . fst) $ deps acc
            when chDebug $ putStrLn "-- accumulated parser results"
            when chDebug $ print acc
            putStrLn "Building modules..."
            b <- pure $ build acc
            putStrLn "Modules are built!"
            when chDebug $ putStrLn "-- Built Modules:"
            when chDebug $ print . Map.keys $ fromRight mempty b
            case b of
                Left s -> fail s
                Right km -> do
                    putStrLn "Saving files:"
                    pb <- PB.newProgressBar PB.defStyle 10 (PB.Progress 0 (Map.size km) ())
                    let load = Data.Bifunctor.first (chOutput </>) <$> Map.toList km
                    saveModules (Just pb) load
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
        printErrorsAndWarnings (zip paths x0)
        let res =
                map (\(p, Right r) -> (p, r)) .
                filter (\(_, q) -> isRight q) . zip paths . map snd $
                x0
        return res
      where
        printErrorsAndWarnings data' = do
            for_ data' $ \(path, (ws, err')) -> do
                when (not (null ws) || isLeft err') $ putStrLn path
                unless (null ws) $ do
                    putStrLn "Warnings:"
                    traverse_ print ws
                when (isLeft err') $ do
                    putStrLn "Errors:"
                    print (head $ lefts [err'])
    saveModules :: Maybe (PB.ProgressBar ()) -> [(FilePath, HsModule')] -> IO ()
    saveModules pb km =
        traverse_
            (\(p, m) -> do
                 saveFile p m
                 when (isJust pb) $ PB.incProgress (fromJust pb) 1)
            km
      where
        saveFile :: String -> HsModule' -> IO ()
        saveFile path md = do
            dflags <- runGhc (Just libdir) getSessionDynFlags
            createDirectoryIfMissing True $ takeDirectory path
            writeFile path $ showPpr dflags md
