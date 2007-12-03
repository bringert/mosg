module Main where

import Mosg

import System.Console.Readline

loop :: Grammar -> Theory -> IO Theory
loop gr th = 
    do minput <- readline "> "
       case minput of
         Nothing -> return th
         Just input -> do addHistory input
                          (out,th') <- handleText gr th input
                          putStrLn $ show out
                          loop gr th'

main :: IO ()
main = do gr <- loadGrammar
          loop gr []
          return ()
