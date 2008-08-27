module Main where

import Mosg

import System.Console.Readline

loop :: Grammar -> Theory -> IO Theory
loop gr th = 
    do minput <- readline "> "
       case minput of
         Nothing -> return th
         Just input -> do addHistory input
                          res <- handleText Pessimistic gr th input
                          let th' = case resOutput res of
                                      AcceptedStatement p -> th ++ [p]
                                      _ -> th
                          putStrLn $ show res
                          loop gr th'

main :: IO ()
main = do gr <- loadGrammar
          loop gr []
          return ()
