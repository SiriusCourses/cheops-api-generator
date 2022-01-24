module FFI(validateJSON, start_python, end_python, withGeneratedBySchema) where

{-# LANGUAGE ForeignFunctionInterface #-}
import Foreign.C             (CBool (..), CString, newCString, peekCString)
import Foreign.Marshal.Alloc (free)
import Data.ByteString.Char8 (ByteString, unpack, pack)

foreign import ccall "unsafe_validate" unsafe_validate :: CString -> CString -> IO CBool
foreign import ccall "start_python" start_python :: IO ()
foreign import ccall "end_python" end_python :: IO ()

foreign import ccall "unsafe_generate" unsafe_generate :: CString -> IO CString
foreign import ccall "dispose_generated_object" dispose_generated_object :: CString -> IO ()

withGeneratedBySchema :: ByteString -> (ByteString -> IO a) -> IO a
withGeneratedBySchema sch k = do
    c_sch <- newCString . unpack $ sch
    c_obj <- unsafe_generate c_sch
    res <- k . pack =<< peekCString c_obj
    dispose_generated_object c_obj
    free c_sch
    return res


validateJSON :: ByteString -> ByteString -> IO Bool
validateJSON obj sch = do 
    c_obj <- newCString . unpack $ obj
    c_sch <- newCString . unpack $ sch
    res <- unsafe_validate c_obj c_sch
    free c_obj
    free c_sch
    return $ res == 1