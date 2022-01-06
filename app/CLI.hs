{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

{-|
Module      : CLI
Description : Module where command line options are specified

Command line properities are specified here. Function which checks if these options are valid is also here.
-}
module CLI
    ( getCLIArgs
    , CheckedInput(..)
    , CheckedPath(..)
    ) where

import Control.Monad    (unless, when)
import Options.Generic
import System.Directory (canonicalizePath, doesDirectoryExist)
import System.Posix     (getFileStatus, isDirectory, isRegularFile)

-- | Command line options defined according to specification in 'Options' package.
data Input =
    Input
        { print_internal_repr :: Bool <?> "If enabled will print to stdout an internal representation of generated code"
        , debug :: Bool <?> "If enabled will print intermediate data into stdout"
        , input :: FilePath <?> "A directory or a file where to start generation from"
        , output :: FilePath <!> "." <?> "A directory where to store generated files"
        , repository_root :: FilePath <!> "/" <?> "Specifies root for absolute paths, defaults to system root"
        , no_modules :: Bool <?> "If enabled no modules are built"
        , no_tests :: Bool <?> "If enabled no tests are built"
        }
    deriving (Generic, Show)

instance ParseRecord Input

-- | Data to store information about either file or directory is specified in command line options
data CheckedPath
    = File FilePath
    | Dir FilePath

-- | Data to store processed command line options.
data CheckedInput =
    CheckedInput
        { 
      -- | Same value as passed in command line arguments
            chPrint_internal_repr :: Bool
      -- | Same value as passed in command line arguments
        , chDebug               :: Bool
      -- | Input path is chekced for existence and is it a directory or a file
        , chInput               :: CheckedPath
      -- | Output path is chekced for existence and is it a directory or a file
        , chOutput              :: FilePath
      -- | Optional root of a repository where json specification is stored. Very useful if there are absolute paths relative to the this root.
        , chRoot                :: FilePath
        , chNoModules           :: Bool
        , chNoTests             :: Bool
        }

-- | Fucntion for getting checked input from command line
getCLIArgs :: IO CheckedInput
getCLIArgs = checkArgs =<< getRecord "cmd args"

-- | Transforms raw input to checked input. Unwrapes type magic and checks if input, output and root paths are correct
checkArgs :: Input -> IO CheckedInput
checkArgs Input {..} = do
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
            (unHelpful no_modules)
            (unHelpful no_tests)
