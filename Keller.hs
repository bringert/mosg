-- | Keller storage
module Keller (Keller, retrieveInput) where

import FOL
import Sem
import GSyntax

import Input

import Control.Applicative (Applicative(..))

type NP = (Exp -> Prop) -> Prop


newtype Keller a = Keller [Store a]

data Store a = Core a
             | Stored (Store (Exp -> a)) (Store NP)

instance Functor Keller where
    fmap f s = pure f <*> s

instance Applicative Keller where
    pure x = Keller [pure x]
    Keller fs <*> Keller xs = Keller [f <*> x | f <- fs, x <- xs]

instance Functor Store where
    fmap f (Core x) = Core (f x)
    fmap f (Stored s np) = Stored (fmap (f .) s) np

instance Applicative Store where
    pure x = Core x
    Core f <*> s = fmap f s
    Stored fs np <*> s = Stored (fmap flip fs <*> s) np


choiceKeller :: [Keller a] -> Keller a
choiceKeller xs = Keller $ concat [ys | Keller ys <- xs]

defaultKeller :: Id a -> Keller a
defaultKeller = Keller . map pure . retrieveId

store :: Keller NP -> Keller NP
store (Keller nps) = Keller [Stored (Core (\z u -> u z)) np | np <- nps]

instance Inter Keller where
    iNP (GDetCN det cn) = let np = iDet det <*> iCN cn in choiceKeller [np, store np]
    iNP np = defaultKeller $ iNP np


--
-- * Retrieval
--

retrieveInput :: InputI Keller -> [Input]
retrieveInput (StatementI p) = map Statement (retrieveKellerProp p)
retrieveInput (QuestionI q) = map Question (retrieveQuestion q)

retrieveQuestion :: QuestI Keller -> [Quest]
retrieveQuestion (YNQuestI p) = map YNQuest (retrieveKellerProp p)
retrieveQuestion (WhQuestI q) = map WhQuest (retrieveKellerPred q)
retrieveQuestion (CountQuestI q) = map CountQuest (retrieveKellerPred q)

retrieveKellerProp :: Keller (Prop) -> [Prop]
retrieveKellerProp = retrieveKeller ($)

retrieveKellerPred :: Keller (Exp -> Prop) -> [Exp -> Prop]
retrieveKellerPred = retrieveKeller (\np f -> np . flip f)

retrieveKeller :: (NP -> (Exp -> a) -> a) -> Keller a -> [a]
retrieveKeller f (Keller ss) = map (retrieveStore f) $ concatMap storePermutations ss

retrieveStore :: (NP -> (Exp -> a) -> a) -> Store a -> a
retrieveStore _ (Core x) = x
retrieveStore f (Stored fs nps) = retrieveStore f (pure f <*> nps <*> fs)

storePermutations :: Store a -> [Store a]
storePermutations (Core x)      = [Core x]
storePermutations (Stored s np) = [ zs | ys <- storePermutations s, zs <- everywhere np ys ]

everywhere :: Store NP -> Store (Exp -> a) -> [Store a]
everywhere x (Core f) = [Stored (Core f) x]
everywhere x (Stored s np) = Stored (Stored s np) x : [ Stored zs np | zs <- everywhere x (fmap flip s) ]
