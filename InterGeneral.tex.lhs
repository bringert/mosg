> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}

> import FOL
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
>   xs@(Cont _) <*> ys@(Cont _) = Cont $ concat 
>       [[\c -> x (\f -> y (\a -> c (f a))),
>         \c -> y (\a -> x (\f -> c (f a)))]
>        | x <- runCont xs, y <- runCont ys]
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
give reset work on non-function types.

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

> type I a = Cont Prop a


> iS :: S -> I Prop
> iS (PredVP np vp) = iNP np <*> iVP vp

Relative clauses are scope islands, and thus use |reset|.

> iRS :: RS -> I (Exp -> Prop)
> iRS (RelVP vp) = reset' (iVP vp)

> iVP :: VP -> I (Exp -> Prop)
> iVP (ComplV2 v2 np) = iV2 v2 <*> iNP np
> iVP (UseV v) = iV v

> iNP :: NP -> I ((Exp -> Prop) -> Prop)
> iNP (DetCN det cn) = iDet det <*> iCN cn
> iNP Everyone  = shift (\c -> forAll (\x -> c (\u -> u x)))
> iNP Someone   = shift (\c -> thereIs (\x -> c (\u -> u x)))
> iNP John      = pure (\f -> f (Const "john"))

> iDet :: Det -> I ((Exp -> Prop) -> (Exp -> Prop) -> Prop)
> iDet Every  = shift (\c -> forAll (\x -> c (\u v -> u x ==> v x)))
> iDet A      = shift (\c -> thereIs (\x -> c (\u v -> u x &&& v x)))

> iCN :: CN -> I (Exp -> Prop)
> iCN (UseN n) = iN n

Is this a scope island?

> iCN (ComplN2 n2 np) = iN2 n2 <*> iNP np
> iCN (RelCN cn rs) = pure (\ci ri x -> ci x &&& ri x) <*> iCN cn <*> iRS rs

> iV :: V -> I (Exp -> Prop)
> iV Sleep = pure (\x -> Pred "sleep" [x])

> iV2 :: V2 -> I (((Exp -> Prop) -> Prop) -> Exp -> Prop)
> iV2 Love = pure (\u x -> u (\y -> Pred "love" [x,y]))

> iN :: N -> I (Exp -> Prop)
> iN Man    = pure (\x -> Pred "man"   [x])
> iN Woman  = pure (\x -> Pred "woman" [x])
> iN Shake  = pure (\x -> Pred "shake" [x])

> iN2 :: N2 -> I (((Exp -> Prop) -> Prop) -> (Exp -> Prop))
> iN2 Owner = pure (\o x -> o (\y -> Pred "owner" [x,y]))


> utt0 = PredVP (DetCN Every (UseN Man)) (UseV Sleep)

> utt1 = PredVP (DetCN Every (UseN Man)) (ComplV2 Love (DetCN A (UseN Woman)))

> utt2 = PredVP (DetCN A (RelCN (UseN Man) (RelVP (ComplV2 Love (DetCN A (UseN Shake)))))) (UseV Sleep)

> utt3 = PredVP John (ComplV2 Love (DetCN Every (ComplN2 Owner (DetCN A (UseN Shake)))))

> utt4 = PredVP (DetCN Every (RelCN (UseN Man) (RelVP (ComplV2 Love (DetCN A (UseN Woman)))))) (UseV Sleep)

> utt5 = PredVP (DetCN A (RelCN (UseN Man) (RelVP (ComplV2 Love (DetCN A (UseN Shake)))))) 
>                  (ComplV2 Love 
>                   (DetCN A (RelCN (UseN Woman) (RelVP (ComplV2 Love (DetCN A (UseN Shake)))))))

> utt6 = PredVP (DetCN Every (ComplN2 Owner (DetCN A (UseN Shake)))) (UseV Sleep)

> utt7 = PredVP (DetCN Every (ComplN2 Owner (DetCN A (UseN Shake)))) 
>               (ComplV2 Love (DetCN A (UseN Woman)))

> test = mapM_ print . {- nub . -} eval . iS