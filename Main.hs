module Main where

import Mosg

import System.Console.Readline

loop :: Grammar -> Theory -> IO Theory
loop gr th = 
    do minput <- readline "> "
       case minput of
         Nothing -> return th
         Just input -> do addHistory input
                          out <- handleText gr th input
                          let th' = case out of
                                      AcceptedStatement p -> th ++ [p]
                                      _ -> th
                          putStrLn $ show out
                          loop gr th'

main :: IO ()
main = do gr <- loadGrammar
          loop gr []
          return ()
