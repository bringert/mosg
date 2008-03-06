module Reason (Theory, Result(..), 
               isTrue, isConsistent, isInformative, areEquivalent,
               prove, counterSatisfy) where

import FOL

import Data.List
import System.Cmd
import System.IO
import Text.Regex.Posix

equinoxPath = "/usr/local/bin/equinox"
paradoxPath = "/usr/local/bin/paradox"


-- Taken from Folkung/Haskell/Form.hs

data Kind
  = Fact
  | NegatedConjecture
  | Conjecture

instance Show Kind where
    show Fact = "axiom"
    show Conjecture = "conjecture"

data Answer
  = Satisfiable
  | CounterSatisfiable
  | Theorem
  | Unsatisfiable
  | Timeout
  | GaveUp
 deriving ( Show, Eq, Ord, Read )


data Result = Yes | No | DontKnow
  deriving (Show, Eq, Ord)

isTrue :: Theory -> Prop -> IO Result
isTrue t p = 
    do x <- prove t p
       case x of
         Yes -> return Yes
         _    -> do y <- prove t (neg p)
                    case y of 
                      Yes -> return No
                      _   -> return DontKnow

isConsistent :: Theory -> Prop -> IO Result
isConsistent th p = counterSatisfy th (neg p)

isInformative :: Theory -> Prop -> IO Result
isInformative th p = counterSatisfy th p

areEquivalent :: Theory -> Prop -> Prop -> IO Result
areEquivalent th p q = isTrue th (p <=> q)

prove :: Theory -> Prop -> IO Result
prove t p = 
    do let input = showProblem t p
       answer <- callTool equinoxPath input
       return $ case answer of
                  Theorem -> Yes
                  _       -> DontKnow

counterSatisfy :: Theory -> Prop -> IO Result
counterSatisfy t p = 
    do let input = showProblem t p
       answer <- callTool paradoxPath input
       return $ case answer of
                  CounterSatisfiable -> Yes
                  Theorem            -> No
                  _                  -> DontKnow

callTool :: String -- ^ tool name
         -> String -- ^ input 
         -> IO Answer
callTool tool input =
    do writeFile "input.p" input
       system $ tool ++ " --time 5 input.p > output.p"
       output <- readFile "output.p"
       let answer = getAnswer output
--       hPutStrLn stderr $ tool ++ " said: " ++ show answer
       return answer

getAnswer :: String -> Answer
getAnswer s = case s =~ "\\+\\+\\+ RESULT: ([[:alnum:]]+)" :: (String,String,String,[String]) of
                (_,_,_,[])  -> error "No +++ RESULT in output"
                (_,_,_,[a]) -> read a
                (_,_,_,_)   -> error "Multiple +++ RESULT in output"

showProblem :: Theory -> Prop -> String
showProblem t p = unlines $ [showFormula Fact (show n) f | (f,n) <- zip t [1..] ]
                            ++ [showFormula Conjecture "input" p]

showFormula :: Kind 
            -> String 
            -> Prop -> String
showFormula r n p = "fof(" ++ n ++ "," ++ show r ++ "," ++ show p ++ ")."
