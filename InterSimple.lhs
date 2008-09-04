%if style == newcode

> module InterSimple (Cont, eval, eval', shift, reset, reset') where

%endif

This is a representation of a non-deterministic continuation monad.

> type Cont o a = [(a -> o) -> o]

> pure :: a -> Cont o a
> pure a = [\c -> c a]


> (<*>) :: Cont o (a -> b) -> Cont o a -> Cont o b
> xs <*> ys = concat 
>       [[\c -> x (\f -> y (\a -> c (f a))),
>         \c -> y (\a -> x (\f -> c (f a)))]
>        | x <- xs, y <- ys]

Run a compuation and retrive all posible results.

> eval :: Cont o o -> [o]
> eval x = [y (\f -> f) | y <- x]

> eval' :: Cont o (a -> o) -> [a -> o]
> eval' x = [\e -> y (\f -> f e) | y <- x]

A computation that looks at the continuation.

> shift :: ((a -> o) -> o) -> Cont o a
> shift f = [f]

The |reset| function for creating delimited continuation computations.

> reset :: Cont o o -> Cont p o
> reset x = [\c -> c y | y <- eval x]

> reset' :: Cont o (a -> o) -> Cont p (a -> o)
> reset' x = [\c -> c y | y <- eval' x]

