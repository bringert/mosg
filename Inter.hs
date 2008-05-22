module Inter (I, (<=*=>), cont, retrieve, reset) where

import FOL
import Input

import Control.Applicative
import Control.Monad
import Data.List
import Data.Monoid

--import Data.Foldable
--import Data.Traversable

newtype Cont a = Cont { runCont :: (a -> Prop) -> Prop }

instance Functor Cont where
    fmap f x = pure f <*> x

instance Applicative Cont where
    pure a   = Cont ($ a)
    x <*> y  = Cont $ \c -> runCont x $ \f -> runCont y $ \a -> (c (f a))

newtype I a = I [Cont a]

instance Functor I where
    fmap f x = pure f <*> x

instance Applicative I where
    pure a         = I [pure a]
    I xs <*> I ys  = I [x <*> y | x <- xs, y <- ys]

instance Alternative I where
    empty          = I []
    I xs <|> I ys  = I (xs++ys)

instance Monoid (I a) where
    mempty   = empty
    mappend  = (<|>)

(<=*=>) :: Alternative f => f (a -> b) -> f a -> f b
x <=*=> y = x <*> y <|> y <**> x

cont :: ((a -> Prop) -> Prop) -> I a
cont f = I [Cont f]

retrieve :: Run a => I a -> [a]
retrieve (I xs) = map run xs

--shift :: ((a -> Cont Prop) -> Cont Prop) -> Cont a
--shift h = Cont (\c -> runCont (h (\v -> Cont (\c' -> c' (c v)))) id)

--reset :: Cont Prop -> Cont Prop
--reset (Cont m) = Cont $ \c -> c (m id)

class Run a where
    run :: Cont a -> a

instance Run Prop where
    run x = runCont x id

instance Run b => Run (a -> b) where
    run x = \e -> run (fmap ($ e) x)
{-
--    run = lift' run

lift' :: Functor m => (m a -> a) -> (m (b -> a) -> (b -> a))
lift' f x = \e -> f (fmap ($ e) x)

lift'' :: (Traversable f, Applicative g) => (f a -> a) -> f (g a) -> g a
lift'' f x = fmap f (sequenceA x)
-}

reset :: Run a => I a -> I a
reset = I . map pure . retrieve
