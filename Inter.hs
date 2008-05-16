module Inter (I, (<=*=>), cont, retrieve, retrieveFun) where

import FOL
import Input

import Control.Applicative
import Control.Monad
import Data.List
import Data.Monoid

newtype Cont a = Cont { runCont :: (a -> Prop) -> Prop }

newtype I a = I [Cont a]

instance Functor I where
    fmap f x = pure f <*> x

instance Applicative I where
    pure a         = I [Cont ($ a)]
    I xs <*> I ys  = I [Cont $ \c -> runCont x $ \f -> runCont y $ \a -> (c (f a))
                        | x <- xs, y <- ys]

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

retrieve :: I Prop -> [Prop]
retrieve (I xs) = [runCont x id | x <- xs]

retrieveFun :: I (a -> Prop) -> [a -> Prop]
retrieveFun (I xs) = [\e -> runCont x ($ e) | x <- xs]

--shift :: ((a -> Cont Prop) -> Cont Prop) -> Cont a
--shift h = Cont (\c -> runCont (h (\v -> Cont (\c' -> c' (c v)))) id)
