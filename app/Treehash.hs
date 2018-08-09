{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Main where

import Push

import System.Environment

main :: IO ()
main = do
    args <- getArgs

    case args of
        ["--", fp] -> do m <- mkMultiPart fp oneMb ""
                         print m 
        _ -> putStrLn "Usage: treehash-exe +RTS -s -- <filename>"
