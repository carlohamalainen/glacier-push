{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Main (main) where

import Data.String.Conversions (cs)
import Push
import System.Environment
import System.Posix.Files

import TreehashFFI (treehash_FFI)

main :: IO ()
main = do
    args <- getArgs

    case args of
        ["--", fp] -> do m <- mkMultiPart fp oneMb ""
                         putStrLn $ cs $ _multipartFullHash m

                         stat <- getFileStatus fp
                         let lastByte = fromIntegral $ (toInteger $ fileSize stat) - 1

                         th_FFI <- treehash_FFI fp 0 lastByte

                         putStrLn th_FFI

        _ -> putStrLn "Usage: treehash-exe +RTS (other rts opts) -- <filename>"
