module Data.ConfigGen.Parsing.IncludeInjection where

import           Control.Exception (handleJust, throwIO)
import           Data.Foldable     (traverse_)

import Control.Monad              (guard, when)

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import Data.Yaml (ParseException (..), YamlException (..))

import qualified Text.Libyaml as Y
import           Text.Libyaml (Event (..), Style (..), Tag (..))


import           Conduit           (MonadIO (liftIO), MonadResource, await)
import           Data.Conduit      (ConduitM, awaitForever, yield, (.|))
import qualified Data.Conduit.List as CL

import System.Directory     (canonicalizePath)
import System.FilePath      (takeDirectory, (</>))
import System.IO.Error      (ioeGetFileName, ioeGetLocation, isDoesNotExistError)




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