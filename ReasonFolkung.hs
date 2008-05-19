module ReasonFolkung where

import FOL

import Flags
import qualified Form as F
import Name
import ParseProblem
import Parsek

import qualified Equinox.Solve as Equinox (solve)
import qualified Paradox.Solve as Paradox (solve)

import Control.Exception as Exception
import Control.Monad
import Data.List
import qualified Data.Set as Set
import System.IO


data Answer = Yes | No | DontKnow
  deriving (Show, Eq, Ord)

--
-- * API
--

isTrue :: Theory -> Prop -> IO Answer
isTrue t p = 
    do x <- prove t p
       case x of
         Yes -> return Yes
         _    -> do y <- prove t (neg p)
                    case y of 
                      Yes -> return No
                      _   -> return DontKnow

isConsistent :: Theory -> Prop -> IO Answer
isConsistent th p = counterSatisfy th (neg p)

isInformative :: Theory -> Prop -> IO Answer
isInformative th p = counterSatisfy th p

areEquivalent :: Theory -> Prop -> Prop -> IO Answer
areEquivalent th p q = isTrue th (p <=> q)

--
-- * Prop to Form
--

propToForm :: Prop -> F.Form
propToForm = runVars . f
  where 

    f :: Prop -> Vars F.Form
    f a = case a of
            Pred s xs -> return $ prd s (map expToTerm xs)
            And ps    -> liftM (foldr1 (F./\)) $ mapM f ps
            Or ps     -> liftM (foldr1 (F.\/)) $ mapM f ps
            Imp p q   -> liftM2 imp (f p) (f q)
            Equiv p q -> liftM2 F.Equiv (f p) (f q)
            Not p     -> liftM F.nt (f p)
            Equal x y -> return $ F.Atom (expToTerm x F.:=: expToTerm y)
            All b     -> quant F.forAll b
            Exists b  -> quant F.exists b
            TrueProp  -> return $ F.true
            FalseProp -> return $ F.false

    prd s xs = F.Atom (F.prd (name s F.::: ([ F.top | _ <- xs ] F.:-> F.bool)) xs)

    imp p q = (F.nt p) F.\/ q

    quant q b = do x <- getUnique
                   f <- f (b (Var x))
                   return $ q (varSymbol x) f

    expToTerm :: Exp -> F.Term
    expToTerm (Const c) = F.Fun (name c F.::: ([] F.:-> F.top)) []
    expToTerm (Var x)   = F.Var (varSymbol x)

    varSymbol :: String -> F.Symbol
    varSymbol x = name x F.::: F.V F.top

--
-- * Call tools
--

folkungFlags :: Flags
folkungFlags = initFlags { time = Just 1 }

toProblem :: Theory -> Prop -> F.Problem
toProblem th p = [F.Input F.Fact (show n) (propToForm a) | (a,n) <- zip th [0..]]
                 ++ [F.Input F.Conjecture "input" (propToForm p)]

prove :: Theory -> Prop -> IO Answer
prove t p = 
    do let prob = toProblem t p
       hPutStrLn stderr $ "Calling Equinox:"
       hPutStrLn stderr $ show prob
       answer <- Exception.try $ Equinox.solve folkungFlags prob
       hPutStrLn stderr $ "Equinox said: " ++ show answer
       return $ case answer of
                  Right F.Theorem -> Yes
                  _         -> DontKnow

counterSatisfy :: Theory -> Prop -> IO Answer
counterSatisfy t p = 
    do let prob = toProblem t p
       hPutStrLn stderr $ "Calling Paradox:"
       hPutStrLn stderr $ show prob
       answer <- Exception.try $ Paradox.solve folkungFlags prob
       hPutStrLn stderr $ "Paradox said: " ++ show answer
       return $ case answer of
                  Right F.CounterSatisfiable -> Yes
                  Right F.Theorem            -> No
                  _                    -> DontKnow
