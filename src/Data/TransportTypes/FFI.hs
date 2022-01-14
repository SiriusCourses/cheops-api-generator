module Data.TransportTypes.FFI(validateJSON, start_python, end_python) where

{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C             (CBool (..), CString, newCString)
import Foreign.Marshal.Alloc (free)
import Data.ByteString.Char8 (ByteString, unpack)

foreign import ccall "unsafe_validate" unsafe_validate :: CString -> CString -> IO CBool
foreign import ccall "start_python" start_python :: IO ()
foreign import ccall "end_python" end_python :: IO ()

validateJSON :: ByteString -> ByteString -> IO Bool
validateJSON obj sch = do 
    c_obj <- newCString . unpack $ obj
    c_sch <- newCString . unpack $ sch
    res <- unsafe_validate c_obj c_sch
    free c_obj
    free c_sch
    return $ res == 1