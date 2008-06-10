> module Inter (I, cont, retrieve, reset) where

> import FOL
> import Input

> import Control.Applicative
> import Control.Monad
> import Data.List
> import Data.Monoid

> data Cont a = Pure a | Cont ((a -> Prop) -> Prop)

> runCont :: Cont a -> ((a -> Prop) -> Prop)
> runCont (Pure x) = \c -> c x
> runCont (Cont r) = r

> instance Functor Cont where
>   fmap f x = pure f <*> x

We have a special case for |Pure f <*> Pure x| to avoid redundant copies
in the implementation of |<*>| for |I|.

> instance Applicative Cont where
>   pure a = Pure a
>   Pure f <*> Pure x  = Pure (f x)
>   x <*> y = Cont $ \c -> runCont x $ \f -> runCont y $ \a -> c (f a)

> newtype I a = I [Cont a]

> instance Functor I where
>   fmap f x = pure f <*> x

> instance Applicative I where
>   pure a             = I [Pure a]
>   I xs <*> I ys      = I [z | x <- xs, y <- ys, z <- app x y]
>     where
>       app :: Cont (a -> b) -> Cont a -> [Cont b]
>       app x@(Cont _) y@(Cont _)  = [x <*> y, y <**> x]
>       app x y                    = [x <*> y]

> cont :: ((a -> Prop) -> Prop) -> I a
> cont f = I [Cont f]

> reset :: Run a => I a -> I a
> reset x = I [pure y | y <- retrieve x]

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

