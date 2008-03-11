import Mosg
import FraCaS
import Problem
import ResultXHtml

import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import System.Environment
import Text.Printf

putRule = putStrLn "----------------------------------------------------------------------"


testProblem :: Grammar -> Problem -> IO ProblemResult
testProblem gr p = 
    do putRule
       putStrLn $ "FraCaS problem " ++ problemId p
       (prs,th) <- addPremises [] (problemPremises p)
       qr <- handleText gr th (problemQuestion p)
       return $ ProblemResult { 
                    problem = p,
                    premiseResults = prs,
                    questionResult = qr
                  }
  where
    addPremises :: Theory -> [String] -> IO ([Result],Theory)
    addPremises th [] = return ([],th)
    addPremises th (p:ps) = do (res,th') <- addPremise th p
                               (rs,th'') <- addPremises th' ps
                               return (res:rs,th'')

    addPremise :: Theory -> String -> IO (Result,Theory)
    addPremise th s = 
        do res <- handleText gr th s
           let th' = case resOutput res of
                       AcceptedStatement p -> th ++ [p]
                       _                   -> th
           return (res, th')

testProblems :: Grammar -> [Problem] -> IO [ProblemResult]
testProblems gr ps = 
    do rs <- mapM (testProblem gr) ps
       let statLine (s,xs) = s ++ (if null xs then "" else ": " ++ unwords (map (problemId) xs))
       mapM_ (putStrLn . statLine) (statistics rs)
       return rs

main :: IO ()
main = do args <- getArgs
          let keepProblem p | null args = True
                            | otherwise = dropWhile (=='0') (problemId p) `elem` args
          gr <- loadGrammar
          problems <- liftM (filter keepProblem) $ readFraCaS "fracas/fracas.xml"
          rs <- testProblems gr problems
          writeOutput rs