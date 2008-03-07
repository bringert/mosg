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
       let goldYes          = [ p | p <- ps, problemAnswer p == Problem.Yes ]
           goldUnknown      = [ p | p <- ps, isUnknown (problemAnswer p) ]
           goldNo           = [ p | p <- ps, problemAnswer p == Problem.No  ]
           answered         = [ problem r | r <- rs, isJust (getAnswer r)]
           failed           = [ problem r | r <- rs, isNothing (getAnswer r)]
           correctYes       = filterAnswers rs (== Mosg.Yes)      (== Problem.Yes)
           correctUnknown   = filterAnswers rs (== Mosg.DontKnow) isUnknown
           correctNo        = filterAnswers rs (== Mosg.No)       (== Problem.No)
           incorrectYes     = filterAnswers rs (== Mosg.Yes)      (/= Problem.Yes)
           incorrectUnknown = filterAnswers rs (== Mosg.DontKnow) (not . isUnknown)
           incorrectNo      = filterAnswers rs (== Mosg.No)       (/= Problem.No)
           report' = report (length ps) 
       putRule
       report' "answered"          answered
       report' "failed"            failed
       putRule
       report' "correct yes"       correctYes
       report' "correct no"        correctNo
       report' "correct unknown"   correctUnknown
       report' "incorrect yes"     incorrectYes
       report' "incorrect no"      incorrectNo
       report' "incorrect unknown" incorrectUnknown

       let xs = [(problem r,res) | r <- rs, res <- premiseResults r ++ [questionResult r]]
           reportError = report (length xs)
       putRule
       reportError "parse errors"            [ p | (p,r) <- xs, resOutput r == NoParse]
       reportError "interpretation errors"   [ p | (p,r) <- xs, resOutput r == NoInterpretation]
       reportError "inconsistent"            [ p | (p,r) <- xs, resOutput r == NoConsistent]

       putRule
       proportion "precision (yes)" correctYes (correctYes ++ incorrectYes)
       proportion "precision (no)" correctNo (correctNo ++ incorrectNo)
       proportion "recall (yes)" correctYes goldYes
       proportion "recall (no)"  correctNo goldNo

       return rs
    where
      filterAnswers rs f g = [ problem r | r <- rs, maybe False f (getAnswer r), g (problemAnswer (problem r))]

      report :: Int -> String -> [Problem] -> IO ()
      report t s xs = printf "%5.1f%% (%3d / %3d) %s: %s\n" (percentage (length xs) t) (length xs) t s (unwords (map (problemId) xs))

      proportion :: String -> [Problem] -> [Problem] -> IO ()
      proportion s xs ys = printf "%5.1f%% (%3d / %3d) %s\n" (percentage (length xs) (length ys)) (length xs) (length ys) s

      percentage :: Int -> Int -> Double
      percentage x t | t == 0 = 0
                     | otherwise = 100 * fromIntegral x / fromIntegral t

main :: IO ()
main = do args <- getArgs
          let keepProblem p | null args = True
                            | otherwise = dropWhile (=='0') (problemId p) `elem` args
          gr <- loadGrammar
          problems <- liftM (filter keepProblem) $ readFraCaS "fracas/fracas.xml"
          rs <- testProblems gr problems
          writeOutput rs