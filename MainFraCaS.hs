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
            | FoundAnswer Result
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
                                      return $ FoundAnswer r
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

isAmbiguous :: Status -> Bool
isAmbiguous AmbiguousQuestion = True
isAmbiguous (AmbiguousPremise _) = True
isAmbiguous _ = False

testProblems :: Grammar -> [Problem] -> IO ()
testProblems gr ps = 
    do rs <- liftM (zip ps) (mapM (testProblem gr) ps)
       putRule
       report "correct answers" [ p | (p,FoundAnswer r) <- rs, isCorrect r (problemAnswer p)]
       report "incorrect answers" [ p | (p,FoundAnswer r) <- rs, not (isCorrect r (problemAnswer p))]
       report "parse errors" [ p | (p,st) <- rs, isParseError st]
       report "interpretation errors" [ p | (p,st) <- rs, isInterpretationError st]
       report "ambiguous" [ p | (p,st) <- rs, isAmbiguous st]
       report "other errors" [ p | (p,OtherError) <- rs]

report :: String -> [Problem] -> IO ()
report s ps = printf "%d %s: %s\n" (length ps) s (show (map problemId ps))

main :: IO ()
main = do args <- getArgs
          let keepProblem p | null args = True
                            | otherwise = problemId p `elem` map read args
          gr <- loadGrammar
          problems <- liftM (filter keepProblem) $ readFraCaS "fracas/fracas.xml"
          testProblems gr problems
