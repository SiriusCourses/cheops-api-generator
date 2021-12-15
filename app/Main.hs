{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards           #-}

module Main where

import           Control.Exception (handleJust, throwIO)
import qualified Data.Bifunctor
import           Data.Coerce       (coerce)
import           Data.Foldable     (Foldable (foldl'), traverse_)
import           Data.String       (IsString (fromString))

import Control.Monad              (guard, when)
import Control.Monad.Except       (runExcept)
import Control.Monad.Reader       (ReaderT (runReaderT))
import Control.Monad.State.Strict (StateT (runStateT))

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import Data.Yaml (FromJSON, ParseException (..), YamlException (..), decodeHelper)

import qualified Text.Libyaml as Y
import           Text.Libyaml (Event (..), Style (..), Tag (..))

import qualified Data.Aeson.Key    as K
import qualified Data.Aeson.KeyMap as KM
import           Data.Aeson.Types  (FromJSON (parseJSON))

import           Conduit           (MonadIO (liftIO), MonadResource, await)
import           Data.Conduit      (ConduitM, awaitForever, yield, (.|))
import qualified Data.Conduit.List as CL

import System.Directory     (canonicalizePath, createDirectoryIfMissing)
import System.FilePath      (takeDirectory, (</>))
import System.FilePath.Find (always, extension, find, (==?))
import System.IO.Error      (ioeGetFileName, ioeGetLocation, isDoesNotExistError)

import GHC           (getSessionDynFlags, runGhc)
import GHC.Paths     (libdir)
import GHC.SourceGen (HsModule', showPpr)

import qualified CLI
import           Data.ConfigGen.Parsing  (ParserResult (..))
import           Data.ConfigGen.Traverse (Dep (ToBuild), GeneratorState (GeneratorState),
                                          modulePartsToModules)

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

-- let makeOne :: GeneratedModules -> IO () =
--         (\v ->
--              traverse_
--                  (\(nm, md) -> do
--                       print nm
--                       putStrLn ""
--                       runGhc (Just libdir) $ putPpr md
--                       putStrLn "")
--                  (Data.Bifunctor.first Text.pack <$> unGeneratedModules v))
eventsFromFile :: MonadResource m => FilePath -> ConduitM i Event m ()
eventsFromFile = go [] []
  where
    go :: MonadResource m => [Event] -> [FilePath] -> FilePath -> ConduitM i Event m ()
    go injectedEvents seen fp = do
        cfp <- liftIO $ handleNotFound $ canonicalizePath fp
        when (cfp `elem` seen) $ liftIO $ throwIO CyclicIncludes
        Y.decodeFile cfp .| conduitInjector injectedEvents .| do
            awaitForever $ \event ->
                case event of
                    EventScalar f (UriTag "!include") _ _ -> do
                        let includeFile = takeDirectory cfp </> T.unpack (T.decodeUtf8 f)
                        let injectedEvents' =
                                [ EventScalar "haskell/origin" NoTag Plain Nothing
                                , EventScalar
                                      (T.encodeUtf8 . T.pack $ includeFile)
                                      NoTag
                                      Plain
                                      Nothing
                                ]
                        go injectedEvents' (cfp : seen) includeFile .|
                            CL.filter (`notElem` irrelevantEvents)
                    _ -> yield event
    irrelevantEvents = [EventStreamStart, EventDocumentStart, EventDocumentEnd, EventStreamEnd]
    handleNotFound :: IO a -> IO a
    handleNotFound =
        handleJust
            (\e -> do
                 guard (isDoesNotExistError e)
                 guard (ioeGetLocation e == "canonicalizePath")
                 ioeGetFileName e)
            (throwIO . YamlException . ("Yaml file not found: " ++))
    conduitInjector :: (Monad m) => [Event] -> ConduitM Event Event m ()
    conduitInjector els = do
        one <- await
        case one of
            Nothing -> return ()
            Just one'@EventMappingStart {} -> do
                yield one'
                traverse_ yield els
                -- turn yourself into id
                awaitForever yield
            Just one' -> do
                yield one'
                -- recourse on yourself
                conduitInjector els
