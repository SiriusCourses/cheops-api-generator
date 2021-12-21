{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards           #-}

module Main where

import qualified Data.Bifunctor
import           Data.Foldable  (Foldable (foldl'), traverse_)
import           Data.String    (IsString (fromString))

import Data.Yaml (decodeHelper)

import qualified Data.Aeson.Key    as K
import qualified Data.Aeson.KeyMap as KM

import System.Directory     (createDirectoryIfMissing)
import System.FilePath      (takeDirectory, (</>))
import System.FilePath.Find (always, extension, find, (==?))

import GHC           (getSessionDynFlags, runGhc)
import GHC.Paths     (libdir)
import GHC.SourceGen (HsModule', showPpr)

import qualified CLI
import           Data.ConfigGen.Parsing 
import           Data.ConfigGen.Traverse

import Control.Monad                           (when)
import Data.ConfigGen.Parsing.IncludeInjection (eventsFromFile)
import Data.Either                             (isLeft, isRight, fromRight)

newtype GeneratedModules =
    GeneratedModules
        { unGeneratedModules :: [(String, HsModule')]
        }
    deriving newtype (Semigroup, Monoid)

collectFiles :: FilePath -> IO [FilePath]
collectFiles = find always (extension ==? ".yaml")

main :: IO ()
main = do
    CLI.CheckedInput {..} <- CLI.getCLIArgs
    files <-
        case chInput of
            CLI.File s -> return [s]
            CLI.Dir s  -> collectFiles s
    traverse_ putStrLn files
    content <-
        sequence <$> traverse (\t -> decodeHelper @ParserResult (eventsFromFile t)) files
    case content of
        Left pe -> print pe
        Right x0 -> do
            res <- extractParsedModules x0 files
            when chDebug $ print "-- resulting parser output:"
            when chDebug $ print res
            when (null res) $
                fail
                    ("No files are successfully parsed. Content looks like this: " ++
                     show content)
            let ini = snd . head $ res
            let acc = replaceDashesWithUnderscores . postprocessParserResult . foldl' combineParserResults ini $ tail res
            when chDebug $ print "-- accumulated parser results"
            when chDebug $ print acc
            let b = build acc
            when chDebug $ print "-- Built Modules:"
            when chDebug $ print . KM.keys $ fromRight mempty b
            case b of
                Left s -> fail s
                Right km ->
                    saveModule $
                    Data.Bifunctor.first ((chOutput </>) . K.toString) <$> KM.toList km
  where
    combineParserResults :: ParserResult -> (FilePath, ParserResult) -> ParserResult
    combineParserResults (ParserResult mainType deps) (p, ParserResult mainType' deps') =
        ParserResult mainType' newDeps
      where
        newDeps = fromKM . KM.insert (fromString p) mainType $ (toKM deps) <> (toKM deps')
        toKM x = KM.fromList $ Data.Bifunctor.first fromString <$> x
        fromKM x = Data.Bifunctor.first K.toString <$> KM.toList x
    extractParsedModules ::
           Show a
        => [([a], Either String ParserResult)]
        -> [FilePath]
        -> IO [(FilePath, ParserResult)]
    extractParsedModules x0 paths = do
        traverse_
            (\x -> do
                 when (not . null . fst $ x) (print . fst $ x)
                 if isLeft . snd $ x
                     then print x
                     else return ())
            x0
        let res =
                map (\(p, Right r) -> (p, r)) .
                filter (\(_, q) -> isRight q) . zipWith (,) paths . map snd $
                x0
        return res
    saveModule :: [(FilePath, HsModule')] -> IO ()
    saveModule km = traverse_ (uncurry saveFile) $ km
      where
        saveFile :: String -> HsModule' -> IO ()
        saveFile path md = do
            dflags <- runGhc (Just libdir) getSessionDynFlags
            createDirectoryIfMissing True $ takeDirectory path
            writeFile path $ showPpr dflags md
