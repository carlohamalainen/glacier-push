{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Main where

import Data.String.Conversions (cs)
import Push
import System.Environment

main :: IO ()
main = do
    args <- getArgs

    case args of
        ["--", fp] -> do m <- mkMultiPart fp oneMb ""
                         putStrLn $ cs $ _multipartFullHash m 
        _ -> putStrLn "Usage: treehash-exe +RTS (other rts opts) -- <filename>"
