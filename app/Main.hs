{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Main where

import Push ( go )

import Control.Exception.Safe ( bracket )
import Control.Monad ( void )
import Katip
import System.Environment ( getArgs )
import System.IO ( stdout )

main :: IO ()
main = do
    args <- getArgs

    case args of
        [vault, path'] -> main' vault path'
        _              -> putStrLn "Usage: glacier-push-exe <vault> <filename>"

  where

    main' :: String -> FilePath -> IO ()
    main' vault path' = do
        handleScribe <- mkHandleScribe ColorIfTerminal stdout (return . const True) V2

        let makeLogEnv = initLogEnv "glacier-push" "production" 
                            >>= 
                         registerScribe "stdout" handleScribe defaultScribeSettings

            initialContext   = ()
            initialNamespace = "main"

        void $ bracket makeLogEnv closeScribes
             $ \le -> runKatipContextT le initialContext initialNamespace
             $ go (Just 64) vault path'
