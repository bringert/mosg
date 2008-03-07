module Input where

import FOL

import Control.Monad
import Text.ParserCombinators.ReadP hiding (get)
import Text.PrettyPrint.HughesPJ hiding (char)

--
-- * External interface
--

data Input = Statement Prop
           | Question Quest

data Quest = YNQuest Prop
           | WhQuest (Exp -> Prop)
           | CountQuest (Exp -> Prop)

instance Eq Input where
    x == y = show x == show y

instance Ord Input where
    compare x y = compare (show x) (show y)

instance Eq Quest where
    x == y = show x == show y

instance Ord Quest where
    compare x y = compare (show x) (show y)

--
-- * Storage interface
--

data InputI i = StatementI (i Prop)
             | QuestionI (QuestI i)

data QuestI i = YNQuestI (i Prop)
              | WhQuestI (i (Exp -> Prop))
              | CountQuestI (i (Exp -> Prop))

negQuestI :: Functor i => QuestI i -> QuestI i
negQuestI = mapQuestI neg

mapInputI :: Functor i => (Prop -> Prop) -> InputI i -> InputI i
mapInputI f (StatementI p) = StatementI (fmap f p)
mapInputI f (QuestionI q)  = QuestionI (mapQuestI f q)

mapQuestI :: Functor i => (Prop -> Prop) -> QuestI i -> QuestI i
mapQuestI f (YNQuestI p)    = YNQuestI (fmap f p)
mapQuestI f (WhQuestI g)    = WhQuestI (fmap (f .) g)
mapQuestI f (CountQuestI g) = CountQuestI (fmap (f .) g)

--
-- * Pretty-printing
--

instance Show Input where
    showsPrec n = showString . render . runVars . pprInput n

instance Show Quest where
    showsPrec n = showString . render . runVars . pprQuestion n

pprInput :: Int -> Input -> Vars Doc
pprInput n (Statement p) = wrapProp "stm" p
pprInput n (Question q) = pprQuestion n q

pprQuestion :: Int -> Quest -> Vars Doc
pprQuestion n (YNQuest p) = wrapProp "ynq" p
pprQuestion n (WhQuest u) = wrapFun "whq" u
pprQuestion n (CountQuest u) = wrapFun "countq" u

wrapProp :: String -> Prop -> Vars Doc
wrapProp s p = liftM ((text s <>) . parens) (pprProp 0 p)

wrapFun :: String -> (Exp -> Prop) -> Vars Doc
wrapFun o u = do v <- getUnique
                 p <- pprProp 0 (u (Var v))
                 return $ text o <> parens (text v <> text "," <> p)

--
-- * Parsing
--

instance Read Input where
    readsPrec n = readP_to_S (readInput n)

readInput :: Int -> ReadP Input
readInput _ = liftM Statement (readWrappedProp "stm")
              +++ liftM Question readQuestion

readQuestion :: ReadP Quest
readQuestion = liftM YNQuest (readWrappedProp "ynq") 
               +++ liftM WhQuest (readWrappedFun "whq")
               +++ liftM CountQuest (readWrappedFun "countq")

readWrappedProp :: String -> ReadP Prop
readWrappedProp s = do skipSpaces
                       string s
                       skipSpaces
                       between (string "(") (string ")") (readProp 0)

readWrappedFun :: String -> ReadP (Exp -> Prop)
readWrappedFun s = do skipSpaces
                      string s
                      skipSpaces
                      string "("
                      v <- readVar
                      string ","
                      p <- readProp 0
                      string ")"
                      return (abstract v p)
