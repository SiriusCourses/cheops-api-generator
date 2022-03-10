module ProjectGen
    ( transferMissingSources
    , createCabalFile
    , cleanUpStackFiles
    , transferCBits
    ) where

import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), MonadTrans (lift), ReaderT)
import Data.Foldable        (for_)
import System.Directory     (canonicalizePath, copyFile, createDirectoryIfMissing,
                             removePathForcibly)
import System.FilePath      (joinPath, takeDirectory, (</>))
import System.Process       (CreateProcess (cwd), createProcess, proc, waitForProcess)

neededFiles :: Bool -> [FilePath]
neededFiles noTests =
    [ joinPath ["external", "Data", "TransportTypes", "ArbitraryInstanceForValue.hs"]
    , joinPath ["external", "Data", "TransportTypes", "Utils.hs"]
    , "package.yaml"
    ] ++
    if noTests
    then []
    else [ joinPath ["test", "ffi", "FFI.hs"]
         , joinPath ["test", "test_prototypes", "Prototypes.hs"]
         ]

cbitsFiles :: Bool -> [FilePath]
cbitsFiles True = []
cbitsFiles _ =
    [ joinPath ["cbits", "c_validate", "unsafe_validate.h"]
    , joinPath ["cbits", "c_validate", "unsafe_validate.cpp"]
    , joinPath ["cbits", "c_validate", "CMakeLists.txt"]
    ]

srcDir :: FilePath
srcDir = "file_templates"

transferCBits :: Bool -> ReaderT FilePath IO ()
transferCBits chNoTests = do
    cTgtDir <- lift . canonicalizePath =<< ask
    lift . for_ (cbitsFiles chNoTests) $ \fl -> do
        createDirectoryIfMissing True $ cTgtDir </> takeDirectory fl
        copyFile fl (cTgtDir </> fl)
    return ()

transferMissingSources :: Bool -> ReaderT FilePath IO ()
transferMissingSources chNoTests = do
    cSrcDir <- lift . canonicalizePath $ srcDir
    cTgtDir <- lift . canonicalizePath =<< ask
    lift . for_ (neededFiles chNoTests) $ \fl -> do
        createDirectoryIfMissing True $ cTgtDir </> takeDirectory fl
        copyFile (cSrcDir </> fl) (cTgtDir </> fl)

createCabalFile :: ReaderT FilePath IO ()
createCabalFile = do
    cTgtDir <- lift . canonicalizePath =<< ask
    (_mb_stdin_hdl, _mb_stdout_hdl, _mb_stderr_hdl, ph) <-
        liftIO $ createProcess (proc "stack" ["build", "--dry-run"]) {cwd = Just cTgtDir}
    _ <- lift $ waitForProcess ph
    return ()

cleanUpStackFiles :: ReaderT FilePath IO ()
cleanUpStackFiles = do
    cTgtDir <- lift . canonicalizePath =<< ask
    liftIO $ removePathForcibly (cTgtDir </> ".stack-work")
    liftIO $ removePathForcibly (cTgtDir </> "stack.yaml")
    liftIO $ removePathForcibly (cTgtDir </> "stack.yaml.lock")
    liftIO $ removePathForcibly (cTgtDir </> "package.yaml")
