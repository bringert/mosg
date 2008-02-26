import Mosg
import FraCaS

import Control.Exception
import Control.Monad
import System.Environment
import Text.Printf


data Status = QuestionParseFailed
            | QuestionInterpretationFailed
            | PremiseParseFailed Int
            | PremiseInterpretationFailed Int
            | AmbiguousQuestion
            | AmbiguousPremise Int
            | FoundAnswer Result Answer
            | OtherError
  deriving Show

putRule = putStrLn "----------------------------------------------------------------------"

testProblem :: Grammar -> Problem -> IO Status
testProblem gr p = 
    do putRule
       putStrLn $ "FraCaS problem " ++ show (problemId p)
       m <- addPremises gr [] (zip [1..] (problemPremises p))
       case m of 
         Left st -> return st
         Right th ->
             do out <- handleText gr th (problemQuestion p)
                case out of
                  YNQAnswer _ r -> do putStrLn $ "Answer: " ++ show r 
                                                   ++ " (correct answer: " ++ show (problemAnswer p) ++ ")"
                                      return $ FoundAnswer r (problemAnswer p)
                  NoParse -> return QuestionParseFailed
                  NoInterpretation _ -> return QuestionInterpretationFailed
                  Ambiguous _ -> return AmbiguousQuestion
                  _ -> return OtherError

addPremises :: Grammar -> Theory -> [(Int,String)] -> IO (Either Status Theory)
addPremises _ th [] = return (Right th)
addPremises gr th (p:ps) = do r <- addPremise gr th p
                              case r of
                                Left st -> return $ Left st
                                Right th' -> addPremises gr th' ps

addPremise :: Grammar -> Theory -> (Int, String) -> IO (Either Status Theory)
addPremise gr th (pid, s) = 
    do out <- handleText gr th s
       case out of
         AcceptedStatement p -> return $ Right $ th ++ [p]
         NoInformative _ -> return $ Right th
         NoParse -> return $ Left $ PremiseParseFailed pid
         NoInterpretation _ -> return $ Left $ PremiseInterpretationFailed pid
         Ambiguous _ -> return $ Left $ AmbiguousPremise pid
         _ -> return $ Left $ OtherError


isCorrect :: Result -> Answer -> Bool
isCorrect Mosg.Yes FraCaS.Yes = True
isCorrect Mosg.No  FraCaS.No  = True
isCorrect DontKnow Unknown = True
isCorrect _ _  = False

isParseError :: Status -> Bool
isParseError QuestionParseFailed = True
isParseError (PremiseParseFailed _) = True
isParseError _ = False

isInterpretationError :: Status -> Bool
isInterpretationError QuestionInterpretationFailed = True
isInterpretationError (PremiseInterpretationFailed _) = True
isInterpretationError _ = False

isAmbigous :: Status -> Bool
isAmbigous AmbiguousQuestion = True
isAmbigous (AmbiguousPremise _) = True
isAmbigous _ = False

testProblems :: Grammar -> [Problem] -> IO ()
testProblems gr ps = 
    do st <- mapM (testProblem gr) ps
       putRule
       printf "%d correct answers\n" $ length [ () | FoundAnswer r a <- st, isCorrect r a]
       printf "%d incorrect answers\n" $ length [ () | FoundAnswer r a <- st, not (isCorrect r a)]
       printf "%d parse errors\n" $ length (filter isParseError st)
       printf "%d interpretation errors\n" $ length (filter isInterpretationError st)
       printf "%d ambiguous\n" $ length (filter isAmbigous st)
       printf "%d other errors\n" $ length [ () | OtherError <- st]

main :: IO ()
main = do args <- getArgs
          let keepProblem p | null args = True
                            | otherwise = problemId p `elem` map read args
          gr <- loadGrammar
          problems <- liftM (filter keepProblem) $ readFraCaS "fracas/fracas.xml"
          testProblems gr problems
