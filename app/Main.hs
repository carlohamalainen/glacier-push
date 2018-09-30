{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Main where

import Push

import Control.Exception.Safe
import Control.Monad
import Katip
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs

    case args of
        [vault, path'] -> main' vault path'
        _              -> putStrLn "Usage: glacier-push-exe <vault> <filename>"

  where

    main' :: String -> FilePath -> IO ()
    main' vault path' = do
        handleScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V2

        let makeLogEnv = do e <- initLogEnv "glacier-push" "production"
                            registerScribe "stdout" handleScribe defaultScribeSettings e

            initialContext   = ()
            initialNamespace = "main"

        void $ bracket makeLogEnv closeScribes
             $ \le -> runKatipContextT le initialContext initialNamespace
             $ go vault path'
