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
             do res <- handleText gr th (problemQuestion p)
                case resOutput res of
                  YNQAnswer r -> do putStrLn $ "Answer: " ++ show r 
                                                 ++ " (correct answer: " ++ show (problemAnswer p) ++ ")"
                                    return $ FoundAnswer r
                  NoParse -> return QuestionParseFailed
                  NoInterpretation -> return QuestionInterpretationFailed
                  Ambiguous -> return AmbiguousQuestion
                  _ -> return OtherError

addPremises :: Grammar -> Theory -> [(Int,String)] -> IO (Either Status Theory)
addPremises _ th [] = return (Right th)
addPremises gr th (p:ps) = do r <- addPremise gr th p
                              case r of
                                Left st -> return $ Left st
                                Right th' -> addPremises gr th' ps

addPremise :: Grammar -> Theory -> (Int, String) -> IO (Either Status Theory)
addPremise gr th (pid, s) = 
    do res <- handleText gr th s
       case resOutput res of
         AcceptedStatement p -> return $ Right $ th ++ [p]
         NoInformative -> return $ Right th
         NoParse -> return $ Left $ PremiseParseFailed pid
         NoInterpretation -> return $ Left $ PremiseInterpretationFailed pid
         NoConsistent -> return $ Left $ PremiseInconsistent pid
         Ambiguous -> return $ Left $ AmbiguousPremise pid
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

isUnknown :: Answer -> Bool
isUnknown FraCaS.Unknown = True
isUnknown FraCaS.Undef = True
isUnknown _ = False

testProblems :: Grammar -> [Problem] -> IO ()
testProblems gr ps = 
    do rs <- liftM (zip ps) (mapM (testProblem gr) ps)
       let goldYes          = [ p | p <- ps, problemAnswer p == FraCaS.Yes ]
           goldUnknown      = [ p | p <- ps, isUnknown (problemAnswer p) ]
           goldNo           = [ p | p <- ps, problemAnswer p == FraCaS.No  ]
           correctYes       = [ p | (p,FoundAnswer Mosg.Yes)      <- rs, problemAnswer p == FraCaS.Yes]
           correctUnknown   = [ p | (p,FoundAnswer Mosg.DontKnow) <- rs, isUnknown (problemAnswer p)]
           correctNo        = [ p | (p,FoundAnswer Mosg.No)       <- rs, problemAnswer p == FraCaS.No ]
           incorrectYes     = [ p | (p,FoundAnswer Mosg.Yes)      <- rs, problemAnswer p /= FraCaS.Yes]
           incorrectUnknown = [ p | (p,FoundAnswer Mosg.DontKnow) <- rs, not (isUnknown (problemAnswer p))]
           incorrectNo      = [ p | (p,FoundAnswer Mosg.No)       <- rs, problemAnswer p /= FraCaS.No]
           failed           = [ p | (p,st) <- rs, not (isFoundAnswer st)]
       putRule
       report "correct yes"       correctYes
       report "correct no"        correctNo
       report "correct unknown"   correctUnknown
       report "incorrect yes"     incorrectYes
       report "incorrect no"      incorrectNo
       report "incorrect unknown" incorrectUnknown
       report "failed"            failed
       putRule
       report "parse errors"            [ p | (p,st) <- rs, isParseError st]
       report "interpretation errors"   [ p | (p,st) <- rs, isInterpretationError st]
       report "inconsistent premises"   [ p | (p,PremiseInconsistent _) <- rs]
       report "ambiguous"               [ p | (p,st) <- rs, isAmbiguous st]
       report "other errors"            [ p | (p,OtherError) <- rs]
       putRule
       proportion "precision (yes)" correctYes (correctYes ++ incorrectYes)
       proportion "precision (no)" correctNo (correctNo ++ incorrectNo)
       proportion "recall (yes)" correctYes goldYes
       proportion "recall (no)"  correctNo goldNo
    where
      report :: String -> [Problem] -> IO ()
      report s xs = printf "%3d (%5.1f%%) %s: %s\n" (length xs) (percentage (length xs) (length ps)) s (show (map problemId xs))

      proportion :: String -> [Problem] -> [Problem] -> IO ()
      proportion s xs ys = printf "%5.1f%% (%3d/%3d) %s\n" (percentage (length xs) (length ys)) (length xs) (length ys) s

      percentage :: Int -> Int -> Double
      percentage x t = 100 * fromIntegral x / fromIntegral t

main :: IO ()
main = do args <- getArgs
          let keepProblem p | null args = True
                            | otherwise = problemId p `elem` map read args
          gr <- loadGrammar
          problems <- liftM (filter keepProblem) $ readFraCaS "fracas/fracas.xml"
          testProblems gr problems
