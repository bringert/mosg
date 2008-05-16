import FOL

import InterShallow
import Input

import Control.Monad.Cont

import Control.Applicative
import Data.List
import Data.Monoid

import Text.PrettyPrint.HughesPJ hiding (char)

--
-- Example abstract syntax
--

data Utt = DeclCl Cl | QuestCl Cl | QuestVP IP VP
  deriving Show

data IP = Who
  deriving Show

data Cl = PredVP NP VP
  deriving Show

data RCl = RelVP VP 
  deriving Show

data VP = ComplV2 V2 NP
        | UseV V
  deriving Show

data NP = DetCN Det CN
        | Everyone
        | Someone
        | John
  deriving Show

data Det = Every | A
  deriving Show

data CN = UseN N
        | ComplN2 N2 NP 
        | RelCN CN RCl
  deriving Show

data V = Sleep
  deriving Show

data V2 = Love
  deriving Show

data N = Man 
        | Woman
        | Shake
  deriving Show

data N2 = Owner
  deriving Show

--
-- Example semantics
--

iUtt :: Utt -> [Input]
iUtt (DeclCl cl) = map Statement $ retrieve $ iCl cl
iUtt (QuestCl cl) = map YNQuest $ retrieve $ iCl cl
iUtt (QuestVP ip vp) = map WhQuest $ retrieveFun $ iIP ip <*> iVP vp

iIP :: IP -> I ((Exp -> Prop) -> (Exp -> Prop))
iIP Who = pure id

iCl :: Cl -> I Prop
iCl (PredVP np vp) = iNP np <*> iVP vp

iRCl :: RCl -> I (Exp -> Prop)
iRCl (RelVP vp) = iVP vp

iVP :: VP -> I (Exp -> Prop)
iVP (ComplV2 v2 np) = iV2 v2 <*> iNP np
iVP (UseV v) = iV v

iNP :: NP -> I ((Exp -> Prop) -> Prop)
iNP (DetCN det cn) = iDet det <*> iCN cn
iNP Everyone = cont $ \c -> forAll  (\x -> c ($ x))
iNP Someone  = cont $ \c -> thereIs (\x -> c ($ x))
iNP John = pure ($ FOL.Const "john")

iDet :: Det -> I ((Exp -> Prop) -> (Exp -> Prop) -> Prop)
iDet Every  = cont (\c -> forAll (\x -> c (\u v -> u x ==> v x)))
iDet A      = cont (\c -> thereIs (\x -> c (\u v -> u x &&& v x)))

iCN :: CN -> I (Exp -> Prop)
iCN (UseN n) = iN n
iCN (ComplN2 n2 np) = iN2 n2 <*> iNP np
iCN (RelCN cn rcl) = pure (\ci ri x -> ci x &&& ri x) <*> iCN cn <*> barrier (iRCl rcl)

iV :: V -> I (Exp -> Prop)
iV Sleep = pure (\x -> Pred "sleep" [x])

iV2 :: V2 -> I (((Exp -> Prop) -> Prop) -> Exp -> Prop)
iV2 Love = pure (\u x -> u (\y -> Pred "love" [x,y]))

iN :: N -> I (Exp -> Prop)
iN Man   = pure (\x -> Pred "man"   [x])
iN Woman = pure (\x -> Pred "woman" [x])
iN Shake = pure (\x -> Pred "shake" [x])

iN2 :: N2 -> I (((Exp -> Prop) -> Prop) -> (Exp -> Prop))
iN2 Owner = pure (\o x -> o (\y -> Pred "owner" [x,y]))

--
-- Testing
--

utt1 = QuestCl (PredVP (DetCN Every (UseN Man)) (ComplV2 Love (DetCN A (UseN Woman))))

utt2 = DeclCl (PredVP (DetCN A (UseN Man)) (ComplV2 Love (DetCN Every (RelCN (UseN Woman) (RelVP (ComplV2 Love (DetCN A (UseN Shake))))))))

utt3 = DeclCl (PredVP John (ComplV2 Love (DetCN Every (ComplN2 Owner (DetCN A (UseN Shake))))))

utt4 = DeclCl (PredVP (DetCN Every (RelCN (UseN Man) (RelVP (ComplV2 Love (DetCN A (UseN Woman)))))) (UseV Sleep)) 

test = mapM_ print . nub . iUtt


--
-- Old stuff
--


{-
data Cont r a = Cont ((a -> r) -> r)
                 | forall b. Apply (Cont r (b -> a)) (Cont r b)

instance Functor (Cont r) where
    fmap f x = pure f <*> x

instance Applicative (Cont r) where
    pure a   = Cont ($ a)
    x <*> y  = Apply x y
-}
{-
newtype Cont r a = Cont { runCont :: (a -> [r]) -> [r] }

instance Functor (Cont r) where
    fmap f x = pure f <*> x

-- Cont is not a monad, since we would need to be able to use the 
-- arguments of bind in any order.
instance Applicative (Cont r) where
    pure a   = Cont ($ a)
    x <*> y  = Cont $ \c ->    (runCont x $ \f -> runCont y $ \a -> (c (f a)))
                            ++ (runCont y $ \a -> runCont x $ \f -> (c (f a)))
-}