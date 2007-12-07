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
                          putStrLn $ answer out
                          loop gr th'

answer :: Output -> String
answer (AcceptedStatement f) = "Accepted " ++ show f
answer NoParse = "Unable to parse input."
answer (NoInterpretation us) = "Unable to interpret input."
answer (NoConsistent ps) = "No consistent interpretation found."
answer (NoInformative ps) = "No informative interpretation found."
answer (Ambiguous is) = "The input is ambiguous."
answer (YNQAnswer q r) = "The answer to " ++ show q ++ " is " ++ show r
answer (WhAnswer q ss) = show (WhQuest q) ++ " = " ++ show ss

main :: IO ()
main = do gr <- loadGrammar
          loop gr []
          return ()
