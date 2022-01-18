module Data.TransportTypes.Parsing.IncludeInjection where

import Control.Exception (handleJust, throwIO)
import Data.Foldable     (traverse_)

import Control.Monad (guard, when)

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import Data.Yaml (ParseException (..), YamlException (..))

import qualified Text.Libyaml as Y
import           Text.Libyaml (Event (..), Style (..), Tag (..))

import           Conduit           (MonadIO (liftIO), MonadResource, await)
import           Data.Conduit      (ConduitM, awaitForever, yield, (.|))
import qualified Data.Conduit.List as CL

import Data.Maybe       (catMaybes)
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath  (isAbsolute, joinPath, splitPath, takeDirectory, (</>))
import System.IO.Error  (ioeGetFileName, ioeGetLocation, isDoesNotExistError)

newtype RepositoryRoot =
    RepositoryRoot FilePath

eventsFromFile :: MonadResource m => RepositoryRoot -> FilePath -> ConduitM i Event m ()
eventsFromFile (RepositoryRoot crr) = go [] []
  where
    go :: MonadResource m => [Event] -> [FilePath] -> FilePath -> ConduitM i Event m ()
    go injectedEvents seen fp = do
        cfp <- liftIO $ handleNotFound $ canonicalizePath =<< rerootPath Nothing fp
        when (cfp `elem` seen) $ liftIO $ throwIO CyclicIncludes
        Y.decodeFile cfp .| conduitInjector injectedEvents .|
            (do awaitForever $ \event ->
                    case event of
                        EventScalar f (UriTag "!include") _ _ -> do
                            includeFile <-
                                liftIO $
                                canonicalizePath =<<
                                rerootPath
                                    (Just $ takeDirectory cfp)
                                    (T.unpack (T.decodeUtf8 f))
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
                        _ -> yield event)
    irrelevantEvents = [EventStreamStart, EventDocumentStart, EventDocumentEnd, EventStreamEnd]
    rerootPath :: Maybe FilePath -> FilePath -> IO FilePath
    rerootPath curDir' fp'
        | Just curDir <- curDir' = do
            exists <- doesFileExist fp'
            if isAbsolute fp' && not exists
                then return rerooted
                else return $ curDir </> fp'
        | Nothing <- curDir' = do
            exists <- doesFileExist fp'
            if isAbsolute fp' && not exists
                then return rerooted
                else return fp'
      where
        rerooted :: FilePath
        rerooted =
            let split_rr = splitPath crr
                split_fp = drop 1 $ splitPath fp' --dropping "/"
             in joinPath $ split_rr ++ split_fp
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
    -- additionalPropertiesNoticer :: (Monad m) => ConduitM Event Event m ()
    -- additionalPropertiesNoticer = do
    --     one <- await
    --     two <- await
    --     three <- await
    --     case (one, two) of
    --         (Nothing, _) -> return ()
    --         (Just one', Nothing) -> do
    --             yield one'
    --             return ()
    --         (Just (EventScalar "additionalProperties" _ _ _), _) ->
    --             case three of
    --                 Nothing -> return ()
    --                 Just three' -> do
    --                     yield three'
    --                     additionalPropertiesNoticer
    --         _ -> do
    --             traverse_ yield $ catMaybes [one, two, three]
    --             additionalPropertiesNoticer
