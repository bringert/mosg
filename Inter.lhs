%if style == newcode

> module Inter (Cont, eval, eval', shift, reset, reset', reset'') where

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

We define the applicative functor operators for our new functor.

> instance Applicative (Cont o) where

The |pure| function of course uses the |Pure| constructor.

>   pure a = Pure [a]

We have a special case for |Pure f <*> Pure x| that keeps the result
pure, so that further uses of |<*>| knows that it
doesn't need both evaluation orders.

If both of the sub-computations are pure, the result is too.

>   Pure fs <*> Pure xs = Pure [f x | f <- fs, x <- xs]

If neither sub-computations is pure, the result
is the cartesian product of the results of the sub-computations,
with either left-to-right or right-to-left evaluation.

>   Cont xs <*> Cont ys = Cont $ concat 
>       [[\c -> x (\f -> y (\a -> c (f a))),
>         \c -> y (\a -> x (\f -> c (f a)))]
>        | x <- xs, y <- ys]

If exacly one sub-computation is pure, then we only need one of the
evaluation orders, since the result is the same regardless of the order.

>   xs <*> ys = Cont [\c -> x (\f -> y (\a -> c (f a))) | x <- runCont xs, y <- runCont ys]

%if style == newcode

Standard |Functor| instance for applicative functors:

> instance Functor (Cont o) where
>   fmap f x = pure f <*> x

%endif

The |eval| operator is very similar to the naive version:

> eval :: Cont o o -> [o]
> eval x = [y (\f -> f) | y <- runCont x]

> eval' :: Cont o (a -> o) -> [a -> o]
> eval' x = [\e -> y (\f -> f e) | y <- runCont x]


> eval'' :: Cont o (a -> b -> o) -> [a -> b -> o]
> eval'' x = [\e d -> y (\f -> f e d) | y <- runCont x]

%
%{
%if style == newcode
%format shift = "shift"
%else 
%format shift = "\xi"
%endif
%
The |shift| operator is straightforward:

> shift :: ((a -> o) -> o) -> Cont o a
> shift f = Cont [f]

%}

And so is the |reset| function for creating delimited continuation computations:

> reset :: Cont o o -> Cont p o
> reset x = Pure (eval x)

> reset' :: Cont o (a -> o) -> Cont p (a -> o)
> reset' x = Pure (eval' x)

> reset'' :: Cont o (a -> b -> o) -> Cont p (a -> b -> o)
> reset'' x = Pure (eval'' x)

