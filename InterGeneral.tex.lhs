> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}

> import FOL
> import Control.Monad
> import Data.List (nub)
> import Text.PrettyPrint.HughesPJ hiding (char)


\section{Applicative Funtors}

> infixl 4 <*>

> class Functor f => Applicative f where
>   pure :: a -> f a
>   (<*>) :: f (a -> b) -> f a -> f b

\section{Non-deterministic Continuation Functor}

> data Cont o a = Pure [a] | Cont [(a -> o) -> o]

> runCont :: Cont o a -> [(a -> o) -> o]
> runCont (Pure xs) = [\c -> c x | x <- xs]
> runCont (Cont r) = r

> instance Functor (Cont o) where
>   fmap f x = pure f <*> x

> instance Applicative (Cont o) where
>   pure a = Pure [a]
>   Pure fs <*> Pure xs = Pure [f x | f <- fs, x <- xs]
>   Cont xs <*> Cont ys = Cont $ concat 
>       [[\c -> x (\f -> y (\a -> c (f a))),
>         \c -> y (\a -> x (\f -> c (f a)))]
>        | x <- xs, y <- ys]
>   xs <*> ys = Cont [\c -> x (\f -> y (\a -> c (f a))) | x <- runCont xs, y <- runCont ys]

> eval :: Cont o o -> [o]
> eval x = [y (\f -> f) | y <- runCont x]

> eval' :: Cont o (a -> o) -> [a -> o]
> eval' x = [\e -> y (\f -> f e) | y <- runCont x]

> shift :: ((a -> o) -> o) -> Cont o a
> shift f = Cont [f]

> reset :: Cont o o -> Cont p o
> reset x = Pure (eval x)

> reset' :: Cont o (a -> o) -> Cont p (a -> o)
> reset' x = Pure (eval' x)

NOTE: this can be used to implemented generic eval and reset, 
but it has the unfortunate side effect of making it hard to 
make reset work on non-function types.

> class Run m where
>   run :: ((m o -> o) -> o) -> m o

> instance Run ((->) a) where
>   run = \y -> \e -> y (\f -> f e)

> instance Functor ((->) a) where
>   fmap f x = \z -> f (x z)

> instance Applicative ((->) a) where
>   pure x = \_ -> x
>   x <*> y = \z -> x z (y z)



\section{Abstract syntax}

> data Utt = UttQS QS | UttS S
>   deriving Show

> data QS = QuestVP IP VP
>         | QuestS S
>   deriving Show

> data IP = Who | IDetCN IDet CN
>   deriving Show

> data IDet = Which | How_many
>   deriving Show

> data S = PredVP NP VP
>   deriving Show

> data RS = RelVP VP 
>   deriving Show

> data VP  = ComplV2 V2 NP | UseV V
>   deriving Show

> data NP  = DetCN Det CN | Everyone | Someone | John
>   deriving Show

> data Det = Every | A
>   deriving Show

> data CN  = UseN N
>          | ComplN2 N2 NP 
>          | RelCN CN RS
>   deriving Show

> data V = Sleep
>   deriving Show

> data V2 = Love
>   deriving Show

> data N = Man | Woman | Shake
>   deriving Show

> data N2 = Owner
>   deriving Show


\section{Semantics}

> data Input = WhQuest (Ind -> Prop)
>            | CountQuest (Ind -> Prop)
>            | YNQuest Prop
>            | Statement Prop

> instance Show Input where
>     showsPrec n = showString . render . runVars . pprInput n

> pprInput :: Int -> Input -> Vars Doc
> pprInput n (Statement p) = wrapProp "stm" p
> pprInput n (YNQuest p) = wrapProp "ynq" p
> pprInput n (WhQuest u) = wrapFun "whq" u
> pprInput n (CountQuest u) = wrapFun "countq" u

> wrapProp :: String -> Prop -> Vars Doc
> wrapProp s p = liftM ((text s <>) . parens) (pprProp 0 p)

> wrapFun :: String -> (Ind -> Prop) -> Vars Doc
> wrapFun o u = do v <- getUnique
>                  p <- pprProp 0 (u (Var v))
>                  return $ text o <> parens (text v <> text "," <> p)

> type I a = Cont Prop a

> iUtt :: Utt -> Cont Input Input
> iUtt (UttQS qs) = iQS qs
> iUtt (UttS s) = pure Statement <*> reset (iS s)

> iQS :: QS -> Cont Input Input
> iQS (QuestVP ip vp) = iIP ip <*> reset' (iVP vp)
> iQS (QuestS s) = pure YNQuest <*> reset (iS s)

> iIP :: IP -> Cont Input ((Ind -> Prop) -> Input)
> iIP Who = pure (\u -> WhQuest u)
> iIP (IDetCN idet cn) = iIDet idet <*> reset' (iCN cn)

> iIDet :: IDet -> Cont Input ((Ind -> Prop) -> (Ind -> Prop) -> Input)
> iIDet Which = pure (\ni u -> WhQuest (\x -> ni x &&& u x))
> iIDet How_many = pure (\ni u -> CountQuest (\x -> ni x &&& u x))

> iS :: S -> I Prop
> iS (PredVP np vp) = iNP np <*> iVP vp

Relative clauses are scope islands, and thus use |reset|.

> iRS :: RS -> I (Ind -> Prop)
> iRS (RelVP vp) = reset' (iVP vp)

> iVP :: VP -> I (Ind -> Prop)
> iVP (ComplV2 v2 np) = iV2 v2 <*> iNP np
> iVP (UseV v) = iV v

> iNP :: NP -> I ((Ind -> Prop) -> Prop)
> iNP (DetCN det cn) = iDet det <*> iCN cn
> iNP Everyone  = shift (\c -> forAll (\x -> c (\u -> u x)))
> iNP Someone   = shift (\c -> thereIs (\x -> c (\u -> u x)))
> iNP John      = pure (\f -> f (Const "john"))

> iDet :: Det -> I ((Ind -> Prop) -> (Ind -> Prop) -> Prop)
> iDet Every  = shift (\c -> forAll (\x -> c (\u v -> u x ==> v x)))
> iDet A      = shift (\c -> thereIs (\x -> c (\u v -> u x &&& v x)))

> iCN :: CN -> I (Ind -> Prop)
> iCN (UseN n) = iN n

Is this a scope island?

> iCN (ComplN2 n2 np) = iN2 n2 <*> iNP np
> iCN (RelCN cn rs) = pure (\ci ri x -> ci x &&& ri x) <*> iCN cn <*> iRS rs

> iV :: V -> I (Ind -> Prop)
> iV Sleep = pure (\x -> Pred "sleep" [x])

> iV2 :: V2 -> I (((Ind -> Prop) -> Prop) -> Ind -> Prop)
> iV2 Love = pure (\u x -> u (\y -> Pred "love" [x,y]))

> iN :: N -> I (Ind -> Prop)
> iN Man    = pure (\x -> Pred "man"   [x])
> iN Woman  = pure (\x -> Pred "woman" [x])
> iN Shake  = pure (\x -> Pred "shake" [x])

> iN2 :: N2 -> I (((Ind -> Prop) -> Prop) -> (Ind -> Prop))
> iN2 Owner = pure (\o x -> o (\y -> Pred "owner" [x,y]))


> utt0 = UttS (PredVP (DetCN Every (UseN Man)) (UseV Sleep))

> utt1 = UttS (PredVP (DetCN Every (UseN Man)) (ComplV2 Love (DetCN A (UseN Woman))))

> utt2 = UttS (PredVP (DetCN A (RelCN (UseN Man) (RelVP (ComplV2 Love (DetCN A (UseN Shake)))))) (UseV Sleep))

> utt3 = UttS (PredVP John (ComplV2 Love (DetCN Every (ComplN2 Owner (DetCN A (UseN Shake))))))

> utt4 = UttS (PredVP (DetCN Every (RelCN (UseN Man) (RelVP (ComplV2 Love (DetCN A (UseN Woman)))))) (UseV Sleep))

> utt5 = UttS (PredVP (DetCN A (RelCN (UseN Man) (RelVP (ComplV2 Love (DetCN A (UseN Shake)))))) 
>                  (ComplV2 Love 
>                   (DetCN A (RelCN (UseN Woman) (RelVP (ComplV2 Love (DetCN A (UseN Shake))))))))

> utt6 = UttS (PredVP (DetCN Every (ComplN2 Owner (DetCN A (UseN Shake)))) (UseV Sleep))

> utt7 = UttS (PredVP (DetCN Every (ComplN2 Owner (DetCN A (UseN Shake)))) 
>               (ComplV2 Love (DetCN A (UseN Woman))))

> utt8 = UttQS (QuestS (PredVP (DetCN Every (UseN Man)) (UseV Sleep)))

> utt9 = UttQS (QuestVP Who (UseV Sleep))

> test = mapM_ print . {- nub . -} eval . iUtt