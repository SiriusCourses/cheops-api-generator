{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C             (CBool (..), CString, newCString)
import Foreign.Marshal.Alloc (free)

foreign import ccall "unsafe_validate" unsafe_validate :: CString -> CString -> IO CBool
foreign import ccall "start_python" start_python :: IO ()
foreign import ccall "end_python" end_python :: IO ()

main :: IO ()
main = do
    start_python
    left <- newCString "{type: string}"
    right <- newCString "{}"
    res <- unsafe_validate left right
    putStrLn ""
    print $ res == 1
    free right
    free left
    end_python
-- x :: LatexRequest
