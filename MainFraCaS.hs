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
            | PremiseInconsistent Int
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
         NoConsistent _ -> return $ Left $ PremiseInconsistent pid
         Ambiguous _ -> return $ Left $ AmbiguousPremise pid
         _ -> return $ Left $ OtherError

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

isFoundAnswer :: Status -> Bool
isFoundAnswer (FoundAnswer _) = True
isFoundAnswer _ = False

testProblems :: Grammar -> [Problem] -> IO ()
testProblems gr ps = 
    do rs <- liftM (zip ps) (mapM (testProblem gr) ps)
       putRule
       report "correct yes"       [ p | (p,FoundAnswer Mosg.Yes)      <- rs, problemAnswer p == FraCaS.Yes]
       report "correct no"        [ p | (p,FoundAnswer Mosg.No)       <- rs, problemAnswer p == FraCaS.No]
       report "correct unknown"   [ p | (p,FoundAnswer Mosg.DontKnow) <- rs, problemAnswer p == FraCaS.Unknown]
       report "incorrect yes"     [ p | (p,FoundAnswer Mosg.Yes)      <- rs, problemAnswer p /= FraCaS.Yes]
       report "incorrect no"      [ p | (p,FoundAnswer Mosg.No)       <- rs, problemAnswer p /= FraCaS.No]
       report "incorrect unknown" [ p | (p,FoundAnswer Mosg.DontKnow) <- rs, problemAnswer p /= FraCaS.Unknown]
       report "failed"            [ p | (p,st) <- rs, not (isFoundAnswer st)]
       putRule
       report "parse errors" [ p | (p,st) <- rs, isParseError st]
       report "interpretation errors" [ p | (p,st) <- rs, isInterpretationError st]
       report "inconsistent premises" [ p | (p,PremiseInconsistent _) <- rs]
       report "ambiguous" [ p | (p,st) <- rs, isAmbiguous st]
       report "other errors" [ p | (p,OtherError) <- rs]
    where
      report :: String -> [Problem] -> IO ()
      report s xs = printf "%3d (%5.1f%%) %s: %s\n" (length xs) (percentage (length xs)) s (show (map problemId xs))
      percentage :: Int -> Double
      percentage x = 100 * fromIntegral x / fromIntegral (length ps)

main :: IO ()
main = do args <- getArgs
          let keepProblem p | null args = True
                            | otherwise = problemId p `elem` map read args
          gr <- loadGrammar
          problems <- liftM (filter keepProblem) $ readFraCaS "fracas/fracas.xml"
          testProblems gr problems
