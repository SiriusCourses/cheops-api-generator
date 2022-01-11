{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C             (CBool (..), CString, newCString)
import Foreign.Marshal.Alloc (free)

foreign import ccall "unsafe_validate" unsafe_validate :: CString -> CString -> IO CBool

main :: IO ()
main = do
    left <- newCString "{type: string}"
    right <- newCString "{}"
    res <- unsafe_validate left right
    putStrLn ""
    print $ res == 1
    free left
    free right
-- x :: LatexRequest
