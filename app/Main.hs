{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards           #-}

module Main where

import Control.Exception (handleJust, throwIO)
import Control.Monad     (guard, when)
import Data.Foldable     (traverse_)

import Data.Yaml (FromJSON, ParseException (..), YamlException (..), decodeHelper)

import qualified Text.Libyaml as Y
import           Text.Libyaml (Event (..), Style (..), Tag (..))

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
import GHC.SourceGen (HsModule', putPpr)

import           Control.Lens.Internal.Coerce (coerce)
import           Control.Monad.Except         (runExcept)
import           Control.Monad.Reader         (ReaderT (runReaderT))
import           Control.Monad.State.Strict   (StateT (runStateT))
import qualified Data.Aeson.Key               as K
import qualified Data.Aeson.KeyMap            as KM
import           Data.Aeson.Types             (FromJSON (parseJSON))
import qualified Data.Bifunctor
import           Data.ConfigGen.Parsing       (ParserResult (..))
import           Data.ConfigGen.Traverse      (Dep (ToBuild), GeneratorState (GeneratorState),
                                               modulePartsToModules)
import           Data.String                  (IsString (fromString))
import qualified Data.Text                    as Text

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
newtype GeneratedModules =
    GeneratedModules
        { unGeneratedModules :: [(String, HsModule')]
        }

instance FromJSON GeneratedModules where
    parseJSON v = do
        ParserResult mainType deps <- parseJSON @ParserResult v
        let q =
                runExcept $
                runStateT (runReaderT (modulePartsToModules mainType) []) $
                GeneratorState . KM.fromList $ Data.Bifunctor.bimap fromString ToBuild <$> deps
        res <-
            case q of
                Left s          -> fail s
                Right (res', _) -> return res'
        return . coerce $ Data.Bifunctor.first K.toString <$> KM.toList res

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
        Right (_, Right (v :: GeneratedModules)) ->
            traverse_
                (\(nm, md) -> do
                     print nm
                     putStrLn ""
                     runGhc (Just libdir) $ putPpr md
                     putStrLn "")
                (Data.Bifunctor.first Text.pack <$> unGeneratedModules v)
         -- Data.ByteString.Lazy.putStr $ encode (v :: ParserResult)
         -- runGhc (Just libdir) . putPpr $ createModule (v :: ParserTypes)
        Right (_, Left e) -> print e

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
(LatexRequest.hs,
 module LatexRequest where
 import /Users/frogofjuly/Documents/Haskell/src/config-generation/models/./ratio.yaml
 data LatexRequest
   = LatexRequest {glossary :: glossary,
                   ratio :: /Users/frogofjuly/Documents/Haskell/src/config-generation/models/./ratio.yaml})
([glossary.hs],
 module Glossary where
 import /Users/frogofjuly/Documents/Haskell/src/config-generation/models/./ratio.yaml
 data glossary
   = glossary {name :: Data.Text.Text,
               nested_ratio :: /Users/frogofjuly/Documents/Haskell/src/config-generation/models/./ratio.yaml,
               size' :: Int,
               somethingelse :: somethingelse})
([glossary/somethingelse.hs],
 module Glossary.Somethingelse where
 data glossary = glossary {another :: Data.Text.Text, one :: Int})
-}
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

type: object
title: ratio
properties:
  num:
    type: number
    haskell/type_info: "Int64"
  denum:
    type: number

--generated:

module Generated where
import qualified GHC.Types
import qualified GHC.Int
import qualified Data.Text
data Glossary
  = Glossary {name :: !Data.Text.Text,
              size :: !GHC.Types.Int,
              somthingelse :: !GHC.Types.Int}
data LatexRequest
  = LatexRequest {glossary :: !Glossary, ratio :: !Ratio}
data Ratio
  = Ratio {denum :: !GHC.Types.Int, num :: !Data.Int.Int64}
])
-}
