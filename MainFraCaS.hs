import Mosg
import FraCaS
import Problem
import ResultXHtml

import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import Text.Printf


data Options = Options {
      optProblems :: [String],
      optMode :: Mode
    }

putRule = putStrLn "----------------------------------------------------------------------"


testProblem :: Options -> Grammar -> Problem -> IO ProblemResult
testProblem opts gr p = 
    do putRule
       putStrLn $ "FraCaS problem " ++ problemId p
       (prs,th) <- addPremises [] (problemPremises p)
       qr <- handleText (optMode opts) gr th (problemQuestion p)
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
        do res <- handleText (optMode opts) gr th s
           let th' = case resOutput res of
                       AcceptedStatement p -> th ++ [p]
                       _                   -> th
           return (res, th')

testProblems :: Options -> Grammar -> [Problem] -> IO [ProblemResult]
testProblems opts gr ps = 
    do rs <- mapM (testProblem opts gr) ps
       let statLine (s,xs) = s ++ (if null xs then "" else ": " ++ unwords (map (problemId) xs))
       mapM_ (putStrLn . statLine) (statistics rs)
       return rs

readProblems :: Options -> FilePath -> IO [Problem]
readProblems opts file = 
    do let keepProblem p | null (optProblems opts) = True
                         | otherwise = dropWhile (=='0') (problemId p) `elem` optProblems opts
       liftM (filter keepProblem) $ readFraCaS file

main :: IO ()
main = do args <- getArgs
          opts <- parseOptions args
          gr <- loadGrammar
          problems <- readProblems opts "syllogisms.xml" --"fracas/fracas.xml"
          rs <- testProblems opts gr problems
          writeOutput (optMode opts) rs

defaultOptions :: Options
defaultOptions = Options {
                   optProblems = [],
                   optMode = Pessimistic
                 }

parseOptions :: [String] -> IO Options
parseOptions args = 
    do let (flags, probs, errs) = getOpt RequireOrder optDescrs args
       when (not (null errs)) $ fail $ unlines errs
       liftM (foldr ($) (defaultOptions { optProblems = probs })) $ sequence flags

optDescrs :: [OptDescr (IO (Options -> Options))]
optDescrs = [Option ['m'] ["mode"] (ReqArg setMode "MODE") "Use MODE, where MODE = Optimistic | Pessimistic"]
  where
    setMode x = readM x >>= \m -> return $ \o -> o { optMode = m }


readM :: (Read a, Monad m) => String -> m a
readM s = case reads s of
                [(x,"")] -> return x
                _        -> fail $ "read failed: " ++ show s

