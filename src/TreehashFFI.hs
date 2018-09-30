{-# LANGUAGE ForeignFunctionInterface #-}

module TreehashFFI (treehash_FFI, treehash_FFI') where

import System.Posix.Files

import Foreign
import Foreign.C.Types
import Foreign.C.String

foreign import ccall "treehash.h treehash"
     c_treehash :: CString -> CULong -> CULong -> IO CString

treehash_FFI :: String -> Int64 -> Int64 -> IO String
treehash_FFI filename start end = withCString filename $ \c_filename -> do
        ptrHash <- c_treehash c_filename (fromIntegral start) (fromIntegral end)
        h <- peekCString ptrHash
        free ptrHash
        return h

treehash_FFI' :: String -> IO String
treehash_FFI' fp = do
    stat <- getFileStatus fp
    let lastByte = fromIntegral $ toInteger (fileSize stat) - 1
    treehash_FFI fp 0 lastByte
