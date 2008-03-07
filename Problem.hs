module Problem where

import Mosg (Result(..))

data Problem = Problem {
                        problemId :: Int,
                        problemPremises :: [Sentence],
                        problemQuestion :: Sentence,
                        problemAnswer :: Answer
                       }
    deriving (Show)

type Sentence = String

data Answer = Yes | No | Unknown | Undef
              deriving (Show,Eq)

data ProblemResult = ProblemResult {
      problem :: Problem,
      premiseResults :: [Result],
      questionResult :: Result
    }
