{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C             (CBool (..), CString, newCString)
import Foreign.Marshal.Alloc (free)

foreign import ccall "unsafe_validate" unsafe_validate :: CString -> CString -> IO CBool

foreign import ccall "unsafe_generate" unsafe_generate :: CString -> IO CString

foreign import ccall "dispose_generated_object" dispose_generated_object :: CString -> IO ()

foreign import ccall "start_python" start_python :: IO ()

foreign import ccall "end_python" end_python :: IO ()

main :: IO ()
main = do
    start_python
    sch <- newCString "{\"type\": \"string\"}"
    obj <- unsafe_generate sch
    -- print =<< peekCString obj
    res <- unsafe_validate obj sch
    putStrLn ""
    print $ res == 1
    dispose_generated_object obj
    free sch
    end_python
