{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Crypto.Hash.SHA256             as SHA256
import qualified Data.ByteString.Base16.Lazy    as B16L
import qualified Data.ByteString.Lazy           as BSL

import Data.Int

import TreehashFFI
import System.Environment
import Data.String.Conversions              (cs)
import System.IO


import qualified Crypto.Hash                    as Hash
import qualified Data.ByteString                as BS
import qualified Data.ByteArray                 as Memory
import qualified Data.ByteString.Base16         as B16

oneMb :: Int64
oneMb = 1024*1024

-- | Compute a tree hash of a bytestring as described in
-- http://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html
treeHashNative :: BSL.ByteString -> BSL.ByteString
treeHashNative s = treeHash' $ leaves s
  where
    treeHash' []  = error "Internal error in treeHash'."
    treeHash' [x] = B16L.encode x
    treeHash' xs  = treeHash' $ next xs

    leaves :: BSL.ByteString -> [BSL.ByteString]
    leaves = map sha256 . oneMbChunks

    next :: [BSL.ByteString] -> [BSL.ByteString]
    next []       = []
    next [a]      = [a]
    next (a:b:xs) = sha256 (BSL.append a b) : next xs

    oneMbChunks :: BSL.ByteString -> [BSL.ByteString]
    oneMbChunks x
      | BSL.length x <= oneMb = [x]
      | otherwise            = BSL.take oneMb x : oneMbChunks (BSL.drop oneMb x)

    sha256 :: BSL.ByteString -> BSL.ByteString
    sha256 = cs . SHA256.hashlazy

-- | Tree hash a file on-disk, avoiding the inefficiency of (cs . readFile).
treeHashNativeOptimized :: FilePath -> IO BS.ByteString
treeHashNativeOptimized filepath = do
    l <- leaves' filepath
    return $ treeHash' l
  where
    treeHash' []  = error "Internal error in treeHash'."
    treeHash' [x] = B16.encode x
    treeHash' xs  = treeHash' $ next xs

    leaves' fp = openFile fp ReadMode >>= loop
      where
        loop h' = do
          eof <- hIsEOF h'
          if eof
            then return []
            else do (h'', chunk) <- hashOneMbChunk h'
                    rest <- loop h''
                    return (Memory.convert chunk : rest)

    chunkSize :: Int64
    chunkSize = 4096

    hashOneMbChunk :: Handle -> IO (Handle, Hash.Digest Hash.SHA256)
    hashOneMbChunk h = loop h Hash.hashInit oneMb
      where
        loop h' context 0 = return (h', Hash.hashFinalize context)
        loop h' !context !remaining = do
          chunk <- BS.hGetSome h' $ fromIntegral chunkSize
          if BS.null chunk
            then return (h', Hash.hashFinalize context)
            else loop h' (Hash.hashUpdate context chunk) (remaining - chunkSize)

    next :: [BS.ByteString] -> [BS.ByteString]
    next []       = []
    next [a]      = [a]
    next (a:b:xs) = sha256 (BS.append a b) : next xs

    sha256 :: BS.ByteString -> BS.ByteString
    sha256 x = Memory.convert (Hash.hash x :: Hash.Digest Hash.SHA256)

main :: IO ()
main = do
    args <- getArgs

    case args of
        ["--ffi", fp]-> do
            th <- treehash_FFI' fp
            putStrLn $ fp ++ " " ++ th

        ["--native", fp] -> do
            th <- cs . treeHashNative <$> BSL.readFile fp
            putStrLn $ fp ++ " " ++ th

        ["--native-optimized", fp] -> do
            th <- cs <$> treeHashNativeOptimized fp
            putStrLn $ fp ++ " " ++ th

        _ -> do
            putStrLn "Usage: treehash-exe +RTS (other rts opts) --native           <filename>"
            putStrLn "Usage: treehash-exe +RTS (other rts opts) --native-optimized <filename>"
            putStrLn "Usage: treehash-exe +RTS (other rts opts) --ffi              <filename>"
