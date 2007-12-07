import Mosg
import FraCaS

import Control.Exception
import Control.Monad


testProblem :: Grammar -> Problem -> IO Bool
testProblem gr p = handle (\e -> putStrLn (show e) >> return False) $
    do putStrLn "-------------------------------------"
       putStrLn $ "FraCaS problem " ++ show (problemId p)
       th <- foldM (addPremise gr) [] (zip [1..] (problemPremises p))
       out <- handleText gr th (problemQuestion p)
       case out of
         YNQAnswer _ r -> do putStrLn $ "Answer: " ++ show r
                             return $ isCorrect r (problemAnswer p)
         _ -> fail $ "Question: " ++ show out

isCorrect :: Result -> Answer -> Bool
isCorrect Mosg.Yes FraCaS.Yes = True
isCorrect Mosg.No  FraCaS.No  = True
isCorrect DontKnow Unknown = True
isCorrect _ _  = False

addPremise :: Grammar -> Theory -> (Int, String) -> IO Theory
addPremise gr th (pid, s) = 
    do out <- handleText gr th s
       case out of
         AcceptedStatement p -> return $ th ++ [p]
         _ -> fail $ "Premise " ++ show pid ++ ": " ++ show out

testProblems :: Grammar -> [Problem] -> IO Int
testProblems gr = liftM (length . filter id) . mapM (testProblem gr)

main :: IO ()
main = do gr <- loadGrammar
          problems <- readFraCaS "fracas/fracas.xml"
          correctCount <- testProblems gr problems
          putStrLn $ "Correct: " ++ show correctCount ++ " of " ++ show (length problems) ++ " problems"
