{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module CLI
    ( getCLIArgs
    , CheckedInput(..)
    , CheckedPath(..)
    ) where

import Control.Monad    (when)
import Options.Generic
import System.Directory (canonicalizePath)
import System.Posix     (getFileStatus, isDirectory, isRegularFile)

data Input =
    Input
        { print_internal_repr :: Bool <?> "If enabled will print to stdout an internal representation of generated code"
        , debug :: Bool <?> "If enabled will print generated code into stdout, ignoring output directory"
        , input :: FilePath <?> "A directory or a file where to start generation from"
        , output :: FilePath <!> "." <?> "A directory where to store generated files"
        }
    deriving (Generic, Show)

instance ParseRecord Input

data CheckedPath
    = File FilePath
    | Dir FilePath

data CheckedInput =
    CheckedInput
        { chPrint_internal_repr :: Bool
        , chDebug               :: Bool
        , chInput               :: CheckedPath
        , chOutput              :: FilePath
        }

getCLIArgs :: IO CheckedInput
getCLIArgs = do
    Input {..} <- getRecord "cmd args"
    inputStatus <- getFileStatus $ unHelpful input
    outputStatus <- getFileStatus $ unDefValue . unHelpful $ output
    when ((not . isDirectory $ inputStatus) && (not . isRegularFile $ inputStatus)) $
        fail "Input file should be file or directory"
    chInput <-
        case isDirectory inputStatus of
            True  -> fmap Dir . canonicalizePath . unHelpful $ input
            False -> fmap File . canonicalizePath . unHelpful $ input
    chOutput <-
        case isDirectory outputStatus of
            False -> fail "output should be a directory"
            True  -> canonicalizePath . unDefValue . unHelpful $ output
    return $ CheckedInput (unHelpful print_internal_repr) (unHelpful debug) chInput chOutput
