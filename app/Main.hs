{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Exception (handleJust, throwIO)
import Control.Monad     (guard, when)
import Data.Foldable     (traverse_)

import Data.Yaml (ParseException (..), YamlException (..), decodeHelper)

import           Text.Libyaml (Event (..), Style (..), Tag (..))
import qualified Text.Libyaml as Y

import           Conduit           (MonadIO (liftIO), MonadResource, await)
import           Data.Conduit      (ConduitM, awaitForever, yield, (.|))
import qualified Data.Conduit.List as CL

import System.Directory   (canonicalizePath)
import System.Environment (getArgs)
import System.FilePath    (takeDirectory, (</>))
import System.IO.Error    (ioeGetFileName, ioeGetLocation, isDoesNotExistError)

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import GHC           (runGhc)
import GHC.Paths     (libdir)
import GHC.SourceGen (putPpr)

import Data.ConfigGen.Parsing (ParsedTypes, createModule)
-- meh
-- constModule :: HsModule'
-- constModule =
--     module'
--         (Just "Generated")
--         (Just [var "meh", var "MyInt"])
--         (qualified' <$> [import' "GHC.Types", import' "GHC.Int", import' "Data.Text"])
--         [ typeSig "meh" $ a --> b --> a
--         , funBind "meh" $ match [x, wildP] x
--         , type' "MyInt" [] int
--         ]
--   where
--     a = var "a"
--     b = var "b"
--     x = bvar "x"
--     int = var "GHC.Types.Int"
-- main :: IO ()
-- main = runGhc (Just libdir) $ putPpr constModule
main :: IO ()
main = do
    filename <- head <$> getArgs
    content <-
        decodeHelper
            (eventsFromFile
                --  "/root/src/config-generation/models/latex-request-object.yaml"
                 filename)
    case content of
        Left pe -> print pe
        Right (_, Right v) ->
            runGhc (Just libdir) . putPpr $ createModule (v :: ParsedTypes)
        Right (_, Left e) -> print e

eventsFromFile :: MonadResource m => FilePath -> ConduitM i Event m ()
eventsFromFile = go [] []
  where
    go :: MonadResource m
       => [Event]
       -> [FilePath]
       -> FilePath
       -> ConduitM i Event m ()
    go injectedEvents seen fp = do
        cfp <- liftIO $ handleNotFound $ canonicalizePath fp
        when (cfp `elem` seen) $ liftIO $ throwIO CyclicIncludes
        Y.decodeFile cfp .| conduitInjector injectedEvents .| do
            awaitForever $ \event ->
                case event of
                    EventScalar f (UriTag "!include") _ _ -> do
                        let includeFile =
                                takeDirectory cfp </> T.unpack (T.decodeUtf8 f)
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
    irrelevantEvents =
        [EventStreamStart, EventDocumentStart, EventDocumentEnd, EventStreamEnd]
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
        -- liftIO . print $ "incoming: " ++ show one
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
{-
-- latex-request-object-inc.yaml
type: object
title: LatexRequest
required:
  - ratio
properties:
  ratio: !include "./ratio.yaml"
  grossery: !include "./some-array.yaml"

--ratio.yaml

type: text
description: |
  "Ratio written in a text form"
example: "1 - 3/4"

--some-array.yaml

- one
- two
- three
- bush
- !include "./brush.yaml"
- !include "./ratio.yaml"

--brush.yaml

"brush"

--generated:

Object (fromList [
    ("properties",Object (fromList [
        ("grossery",Array [
            String "one",
            String "two",
            String "three",
            String "bush",
            String "brush",
            Object (fromList [
                ("description",String "\"Ratio written in a text form\"\n"),
                ("example",String "1 - 3/4"),
                ("haskell/origin",String "/root/src/config-generation/models/./ratio.yaml"),
                ("type",String "text")
            ])
        ]),
        ("ratio",Object (fromList [
            ("description",String "\"Ratio written in a text form\"\n"),
            ("example",String "1 - 3/4"),
            ("haskell/origin",String "/root/src/config-generation/models/./ratio.yaml"),
            ("type",String "text")
        ]))
    ])),
    ("required",Array [String "ratio"]),
    ("title",String "LatexRequest"),
    ("type",String "object")
])
-}