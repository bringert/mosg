import FOL

import Inter

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

data CN = Man 
        | Woman
        | Shake
        | RelCN CN RCl
  deriving Show

data V = Sleep
  deriving Show

data V2 = Love
  deriving Show

--
-- Example semantics
--

data Input = Statement Prop | YNQ Prop | WhQ (Exp -> Prop)

instance Eq Input where
    x == y = show x == show y

instance Ord Input where
    compare x y = compare (show x) (show y)

instance Show Input where
    showsPrec n = showString . render . runVars . pprInput n

pprInput :: Int -> Input -> Vars Doc
pprInput n (Statement p) = wrapProp "stm" p
pprInput n (YNQ p) = wrapProp "ynq" p
pprInput n (WhQ u) = wrapFun "whq" u

wrapProp :: String -> Prop -> Vars Doc
wrapProp s p = liftM ((text s <>) . parens) (pprProp 0 p)

wrapFun :: String -> (Exp -> Prop) -> Vars Doc
wrapFun o u = do v <- getUnique
                 p <- pprProp 0 (u (Var v))
                 return $ text o <> parens (text v <> text "," <> p)



iUtt :: Utt -> [Input]
iUtt (DeclCl cl) = map Statement $ retrieve $ iCl cl
iUtt (QuestCl cl) = map YNQ $ retrieve $ iCl cl
iUtt (QuestVP ip vp) = map WhQ $ retrieveFun $ iIP ip <*> iVP vp

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
iDet Every =     pure (\u v -> forAll (\x -> u x ==> v x))
             <|> cont (\c -> forAll (\x -> c (\u v -> u x ==> v x)))
iDet A =     pure (\u v -> thereIs (\x -> u x ==> v x))
         <|> cont (\c -> thereIs (\x -> c (\u v -> u x ==> v x)))

iCN :: CN -> I (Exp -> Prop)
iCN Man   = pure (\x -> Pred "man"   [x])
iCN Woman = pure (\x -> Pred "woman" [x])
iCN Shake = pure (\x -> Pred "shake" [x])
iCN (RelCN cn rcl) = pure (\ci ri x -> ci x &&& ri x) <*> iCN cn <*> iRCl rcl

iV :: V -> I (Exp -> Prop)
iV Sleep = pure (\x -> Pred "sleep" [x])

iV2 :: V2 -> I (((Exp -> Prop) -> Prop) -> Exp -> Prop)
iV2 Love = pure (\u x -> u (\y -> Pred "love" [x,y]))


--
-- Testing
--

utt1 = QuestCl (PredVP (DetCN Every Man) (ComplV2 Love (DetCN A Woman)))

utt2 = DeclCl (PredVP (DetCN A Man) (ComplV2 Love (DetCN Every (RelCN Woman (RelVP (ComplV2 Love (DetCN A Shake)))))))

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