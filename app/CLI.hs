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

import Control.Monad    (unless, when)
import Options.Generic
import System.Directory (canonicalizePath, doesDirectoryExist)
import System.Posix     (getFileStatus, isDirectory, isRegularFile)

data Input =
    Input
        { print_internal_repr :: Bool <?> "If enabled will print to stdout an internal representation of generated code"
        , debug :: Bool <?> "If enabled will print generated code into stdout, ignoring output directory"
        , input :: FilePath <?> "A directory or a file where to start generation from"
        , output :: FilePath <!> "." <?> "A directory where to store generated files"
        , repository_root :: FilePath <!> "/" <?> "Specifies root for absolute paths, defaults to system root"
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
        , chRoot                :: FilePath
        }

getCLIArgs :: IO CheckedInput
getCLIArgs = do
    Input {..} <- getRecord "cmd args"
    inputStatus <- getFileStatus $ unHelpful input
    outputStatus <- getFileStatus $ unDefValue . unHelpful $ output
    rootExists <- doesDirectoryExist $ unDefValue . unHelpful $ repository_root
    unless rootExists . fail $
        "repository root must exist. This: \"" ++
        show (unDefValue . unHelpful $ repository_root) ++ "\" does not."
    when ((not . isDirectory $ inputStatus) && (not . isRegularFile $ inputStatus)) $
        fail "Input file should be file or directory"
    chInput <-
        if isDirectory inputStatus
            then fmap Dir . canonicalizePath . unHelpful $ input
            else fmap File . canonicalizePath . unHelpful $ input
    chOutput <-
        if isDirectory outputStatus
            then canonicalizePath . unDefValue . unHelpful $ output
            else fail "output should be a directory"
    return $
        CheckedInput
            (unHelpful print_internal_repr)
            (unHelpful debug)
            chInput
            chOutput
            (unDefValue . unHelpful $ repository_root)
