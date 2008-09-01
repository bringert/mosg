%if style == newcode

> module Inter (I, cont, retrieve, reset) where

> import FOL

> import Control.Applicative
> import Control.Monad
> import Data.List
> import Data.Monoid

%endif

This is a standard representation of a continuation monad,
but with an added constructor (|Pure|) for computations which do
not look at the continuation.

> data Cont a = Pure a | Cont ((a -> Prop) -> Prop)

Getting a function that looks at the continuation from
a |Cont| is easy:

> runCont :: Cont a -> ((a -> Prop) -> Prop)
> runCont (Pure x) = \c -> c x
> runCont (Cont r) = r

> instance Applicative Cont where
>   pure a = Pure a

We have a special case for |Pure f <*> Pure x| that keeps the result
pure, so that the implementation of |<*>| for |I| knows that it only
needs 

>   Pure f <*> Pure x  = Pure (f x)

Standard left-to-right evaluation of combined continuation
computations.

>   x <*> y = Cont $ \c -> runCont x $ \f -> runCont y $ \a -> c (f a)

Standard |Functor| instance for applicative functors:

> instance Functor Cont where
>   fmap f x = pure f <*> x

We now introduce a type of non-deterministic continuation computations.

> newtype I a = I [Cont a]

> instance Applicative I where
>   pure a             = I [Pure a]

When combining two non-deterministic computations, we take
all possible combinations of results from the sub-computations,
and combine them. 

>   I xs <*> I ys      = I [z | x <- xs, y <- ys, z <- app x y]
>     where
>       app :: Cont (a -> b) -> Cont a -> [Cont b]

When combining two non-pure sub-computations,
we allow either left-to-right or right-to-left evaluation order.

>       app x@(Cont _) y@(Cont _)  = [x <*> y, y <**> x]
>       app x y                    = [x <*> y]

Standard |Functor| instance for applicative functors:

> instance Functor I where
>   fmap f x = pure f <*> x

A computation that looks at the continuation.

> cont :: ((a -> Prop) -> Prop) -> I a
> cont f = I [Cont f]

The |reset| function for creating delimited continuation computations.

> reset :: Run a => I a -> I a
> reset x = I [pure y | y <- retrieve x]

Runs a non-deterministic continuation computation and retrieves the
results.

> retrieve :: Run a => I a -> [a]
> retrieve (I xs) = map run xs

> class Run a where
>   run :: Cont a -> a

> instance Run Prop where
>   run x = runCont x id

> instance Run b => Run (a -> b) where
>   run x = \e -> run (fmap (\f -> f e) x)

> instance Alternative I where
>   empty          = I []
>   I xs <|> I ys  = I (xs++ys)
