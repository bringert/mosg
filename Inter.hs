module Inter (I, cont, barrier, retrieve, retrieveFun) where

import FOL

import Control.Monad.Cont

import Control.Applicative
import Data.List
import Data.Monoid

-- I is not a monad, since we would need to be able to use the 
-- arguments of bind in any order.
newtype I a = I [Cont Prop a]

instance Functor I where
    fmap f x = pure f <*> x

instance Applicative I where
    pure a         = I [Cont ($ a)]
    I xs <*> I ys  = I $ map Cont $ concat
                         [[\c -> runCont x $ \f -> runCont y $ \a -> (c (f a)),
                           \c -> runCont y $ \a -> runCont x $ \f -> (c (f a))]
                           | x <- xs, y <- ys]

instance Alternative I where
    empty          = I []
    I xs <|> I ys  = I (xs++ys)

instance Monoid (I a) where
    mempty   = empty
    mappend  = (<|>)

cont :: ((a -> Prop) -> Prop) -> I a
cont f = I [Cont f]

barrier :: I (Exp -> Prop) -> I (Exp -> Prop)
barrier = I . map return . retrieveFun

retrieve :: I Prop -> [Prop]
retrieve (I xs) = [runCont x id | x <- xs]

retrieveFun :: I (Exp -> Prop) -> [Exp -> Prop]
retrieveFun (I xs) = [\e -> runCont x ($ e) | x <- xs]
