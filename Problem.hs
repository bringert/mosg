module Problem where

import Mosg (Result(..), Output(..), Answer(..))

data Problem = Problem {
                        problemId :: String,
                        problemPremises :: [Sentence],
                        problemQuestion :: Sentence,
                        problemAnswer :: Problem.Answer
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


getAnswer :: ProblemResult -> Maybe Mosg.Answer
getAnswer r | any premiseFailed (premiseResults r) = Nothing
            | otherwise = case resOutput (questionResult r) of
                            YNQAnswer a -> Just a
                            _           -> Nothing

premiseFailed :: Result -> Bool
premiseFailed res = case resOutput res of
                      AcceptedStatement _ -> False
                      NoInformative       -> False
                      _                   -> True

isUnknown :: Problem.Answer -> Bool
isUnknown Problem.Unknown = True
isUnknown Problem.Undef = True
isUnknown _ = False

isCorrect :: ProblemResult -> Maybe Bool
isCorrect r = case (getAnswer r, problemAnswer (problem r)) of
                (Nothing,            _          ) -> Nothing
                (Just Mosg.Yes,      Problem.Yes) -> Just True
                (Just Mosg.No,       Problem.No ) -> Just True
                (Just Mosg.DontKnow, x          ) -> Just  (isUnknown x)
                (_,                  _          ) -> Just False
