%if style == newcode

> module Inter (Cont, eval, eval', shift, reset, reset') where

> import Control.Applicative (Applicative(..))
> import Control.Monad
> import Data.List

%endif

This is a representation of a non-deterministic continuation monad,
with an added constructor (|Pure|) for computations which do
not look at the continuation.

> data Cont o a = Pure [a] | Cont [(a -> o) -> o]

Getting a function that looks at the continuation from
a |Cont| is easy:

> runCont :: Cont o a -> [(a -> o) -> o]
> runCont (Pure xs) = [\c -> c x | x <- xs]
> runCont (Cont r) = r

> instance Applicative (Cont o) where
>   pure a = Pure [a]

We have a special case for |Pure f <*> Pure x| that keeps the result
pure, so that |<*>| knows that it
doesn't need both evaluation orders.

>   Pure fs <*> Pure xs = Pure [f x | f <- fs, x <- xs]
>   Cont xs <*> Cont ys = Cont $ concat 
>       [[\c -> x (\f -> y (\a -> c (f a))),
>         \c -> y (\a -> x (\f -> c (f a)))]
>        | x <- xs, y <- ys]
>   xs <*> ys = Cont [\c -> x (\f -> y (\a -> c (f a))) | x <- runCont xs, y <- runCont ys]

Standard |Functor| instance for applicative functors:

> instance Functor (Cont o) where
>   fmap f x = pure f <*> x

> eval :: Cont o o -> [o]
> eval x = [y (\f -> f) | y <- runCont x]

> eval' :: Cont o (a -> o) -> [a -> o]
> eval' x = [\e -> y (\f -> f e) | y <- runCont x]

A computation that looks at the continuation.

%{
%if style == newcode
%format shift = "shift"
%else 
%format shift = "\xi"
%endif

> shift :: ((a -> o) -> o) -> Cont o a
> shift f = Cont [f]

%}

The |reset| function for creating delimited continuation computations.

> reset :: Cont o o -> Cont p o
> reset x = Pure (eval x)

> reset' :: Cont o (a -> o) -> Cont p (a -> o)
> reset' x = Pure (eval' x)

