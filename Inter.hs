{-# LANGUAGE ExistentialQuantification #-}
module Inter (I, cont, barrier, retrieve, retrieveFun) where

import FOL
import Input

import Control.Applicative
import Control.Monad
import Data.List
import Data.Monoid

-- Deep embedding

type I a = Cont Prop a

data Cont r a = Pure [a]
--              | Choice [Cont r a]
              | C [(a -> r) -> r]
              | forall b. Apply (Cont r (b -> a)) (Cont r b)

instance Functor (Cont r) where
    fmap f x = pure f <*> x

instance Applicative (Cont r) where
    pure a   = Pure [a]
    x <*> y  = Apply x y

runCont :: Cont r a -> [(a -> r) -> r]
runCont (Pure xs) = [\c -> c x | x <- xs]
runCont (C fs) = fs
runCont (Apply x y) | isPure x || isPure y =
     [\c -> a (\a' -> b (\b' -> c (a' b')))
           | a <- runCont x, b <- runCont y]
runCont (Apply x y) = 
    concat [[\c -> a (\a' -> b (\b' -> c (a' b'))),
             \c -> b (\b' -> a (\a' -> c (a' b')))] 
           | a <- runCont x, b <- runCont y]

isPure :: Cont r a -> Bool
isPure (Pure _) = True
isPure (Apply x y) = isPure x && isPure y
isPure _ = False

cont :: ((a -> Prop) -> Prop) -> I a
cont f = C [f]

barrier :: I (Exp -> Prop) -> I (Exp -> Prop)
barrier i = Pure (retrieveFun i)

retrieve :: I Prop -> [Prop]
retrieve = map ($ id) . runCont

retrieveFun :: I (a -> Prop) -> [a -> Prop]
retrieveFun i = [\e -> f ($ e) | f <- runCont i]

{-
shift :: ((a -> I Prop) -> I Prop) -> I a
shift h = C [\c -> runCont (h (\v -> C [\c' -> c' (c v)])) id]
-}

reset :: I Prop -> I Prop
reset i = Pure [f id | f <- runCont i]

resetFun :: I (Exp -> Prop) -> I (Exp -> Prop)
resetFun i = Pure [\e -> f ($ e) | f <- runCont i]

{-

-- Shallow embedding

newtype Cont a = Cont { runCont :: (a -> Prop) -> Prop }

-- I is not a monad, since we would need to be able to use the 
-- arguments of bind in any order.
newtype I a = I [Cont a]

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
barrier i = I [ Cont ($ f) | f <- retrieveFun i]

retrieve :: I Prop -> [Prop]
retrieve (I xs) = [runCont x id | x <- xs]

retrieveFun :: I (a -> Prop) -> [a -> Prop]
retrieveFun (I xs) = [\e -> runCont x ($ e) | x <- xs]

shift :: ((a -> Cont Prop) -> Cont Prop) -> Cont a
shift h = Cont (\c -> runCont (h (\v -> Cont (\c' -> c' (c v)))) id)

reset :: Cont Prop -> Cont Prop
reset (Cont m) = Cont $ \c -> c (m id)

-}