
%if style == newcode
\begin{code}
module Input where

import FOL

import Control.Monad
import Text.ParserCombinators.ReadP hiding (get)
import Text.PrettyPrint.HughesPJ hiding (char)
\end{code}
%endif


\begin{code}
data Input = Statement Prop
           | YNQuest Prop
           | WhQuest (Exp -> Prop)
           | CountQuest (Exp -> Prop)
\end{code}

%if style == newcode
\begin{code}
instance Eq Input where
    x == y = show x == show y

instance Ord Input where
    compare x y = compare (show x) (show y)

mapInput :: (Prop -> Prop) -> Input -> Input
mapInput f (Statement p)  = Statement (f p)
mapInput f (YNQuest p)    = YNQuest (f p)
mapInput f (WhQuest g)    = WhQuest (f . g)
mapInput f (CountQuest g) = CountQuest (f . g)

isStatement :: Input -> Bool
isStatement (Statement _) = True
isStatement _ = False

--
-- * Conversion
--

fromStatement :: Input -> Prop
fromStatement (Statement p) = p

toYNQuest :: Input -> Input
toYNQuest (Statement p) = YNQuest p
toYNQuest (YNQuest p) = YNQuest p

toWhQuest :: (Exp -> Input) -> Input
toWhQuest f = WhQuest (\x -> case f x of { Statement p -> p})

toCountQuest :: (Exp -> Input) -> Input
toCountQuest f = CountQuest (\x -> case f x of { Statement p -> p})

--
-- * Pretty-printing
--

instance Show Input where
    showsPrec n = showString . render . runVars . pprInput n

pprInput :: Int -> Input -> Vars Doc
pprInput n (Statement p) = wrapProp "stm" p
pprInput n (YNQuest p) = wrapProp "ynq" p
pprInput n (WhQuest u) = wrapFun "whq" u
pprInput n (CountQuest u) = wrapFun "countq" u

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
readInput _ = choice [liftM Statement (readWrappedProp "stm"),
                     liftM YNQuest (readWrappedProp "ynq"),
                     liftM WhQuest (readWrappedFun "whq"),
                     liftM CountQuest (readWrappedFun "countq")]

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
\end{code}
%endif
