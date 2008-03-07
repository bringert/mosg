{-# OPTIONS_GHC -Wall -}
module FOL (
            -- * Types
            Theory, Prop(..),Exp(..),
            -- * Construction API
            (&&&), (|||), ands, ors, (==>), (<=>), neg, (===), (=/=), forAll, thereIs, true, false,
            -- * Pretty-printing internals
            Vars, runVars, getUnique,
            pprProp,
            -- * Parsing internals
            readProp, readVar, abstract
            ) 
    where

import Control.Monad
import Data.Char
import Data.List
import Text.ParserCombinators.ReadP hiding (get)
import Text.PrettyPrint.HughesPJ hiding (char)

--
-- * Types
--

type Theory = [Prop]

data Prop =
    Pred String [Exp]
  | And [Prop]
  | Or [Prop]
  | Imp Prop Prop
  | Equiv Prop Prop
  | Not Prop
  | Equal Exp Exp
  | All (Exp -> Prop)
  | Exists (Exp -> Prop)
  | TrueProp
  | FalseProp

data Exp = Const String
         | Var Var

type Var = String


--
-- * API
--

infixr 1 ==>
infixr 2 |||
infixr 3 &&&
infix  4 ===, =/=

(&&&) :: Prop -> Prop -> Prop
p &&& q = simplify $ And [p,q]

ands :: [Prop] -> Prop
ands = simplify . And

ors :: [Prop] -> Prop
ors = simplify . Or

(|||) :: Prop -> Prop -> Prop
p ||| q = simplify $ Or [p,q]

(==>) :: Prop -> Prop -> Prop
p ==> q = simplify $ Imp p q

(<=>) :: Prop -> Prop -> Prop
p <=> q = simplify $ Equiv p q

neg :: Prop -> Prop
neg p = simplify $ Not p

(===) :: Exp -> Exp -> Prop
(===) = Equal

(=/=) :: Exp -> Exp -> Prop
x =/= y = neg (x === y)

forAll :: (Exp -> Prop) -> Prop
forAll f = simplify $ All f

thereIs :: (Exp -> Prop) -> Prop
thereIs f = simplify $ Exists f

true :: Prop
true = TrueProp

false :: Prop
false = FalseProp

simplify :: Prop -> Prop
simplify (And []) = TrueProp
simplify (And ps) = And [simplify p | p <- flattenAnd ps, not (isTrueProp p)]
simplify (Or []) = FalseProp
simplify (Or ps) = Or [simplify p | p <- flattenOr ps, not (isFalseProp p)]
simplify (Imp p q) = Imp (simplify p) (simplify q)
simplify (Equiv p q) = Equiv (simplify p) (simplify q)
simplify (Not p) = Not (simplify p)
simplify (All f) = All (\x -> simplify (f x))
simplify (Exists f) = Exists (\x -> simplify (f x))
simplify p = p

isTrueProp :: Prop -> Bool
isTrueProp TrueProp = True
isTrueProp _ = False

isFalseProp :: Prop -> Bool
isFalseProp FalseProp = True
isFalseProp _ = False

flattenAnd :: [Prop] -> [Prop]
flattenAnd [] = []
flattenAnd (And qs:ps) = flattenAnd (qs++ps)
flattenAnd (p:ps) = p : flattenAnd ps

flattenOr :: [Prop] -> [Prop]
flattenOr [] = []
flattenOr (Or qs:ps) = flattenOr (qs++ps)
flattenOr (p:ps) = p : flattenOr ps

--
-- * Generating variables names
--

newtype Unique x a = Unique ([x] -> (a,[x]))

instance Monad (Unique x) where
    return x = Unique (\xs -> (x,xs))
    Unique m >>= f = Unique $ \xs -> let (a,xs') = m xs
                                         Unique f' = f a
                                      in f' xs'

runUnique :: [x] -> Unique x a -> a
runUnique xs (Unique f) = fst $ f xs

getUnique :: Unique x x
getUnique = Unique $ \ (x:xs) -> (x,xs)

type Vars a = Unique Var a

runVars :: Vars a -> a
runVars = runUnique vars

vars :: [Var]
vars = tail $ concat $ iterate f [""]
  where f = concatMap (\v -> map ((v++).(:[])) ['A'..'Z'])

--
-- * Syntactic equality
--

instance Eq Prop where
    p == q = show p == show q

instance Ord Prop where
    compare p q = compare (show p) (show q)

--
-- * Pretty-printing with TPTP syntax
--

instance Show Prop where
    showsPrec n = showString . render . runUnique vars . pprProp n

pprProp :: Int -> Prop -> Vars Doc
pprProp _ (Pred x xs) = do xs' <- mapM pprExp xs
                           return $ text x <> parens (hcat (punctuate (text ",") xs'))
pprProp n (And xs)    = liftM (prec 1 n . hsep . intersperse (text "&")) $ mapM (pprProp 1) xs
pprProp n (Or xs)     = liftM (prec 1 n . hsep . intersperse (text "|")) $ mapM (pprProp 1) xs
pprProp n (Imp x y)   = binConn "=>" n x y
pprProp n (Equiv x y) = binConn "<=>" n x y
pprProp n (Equal x y) =  do x' <- pprExp x
                            y' <- pprExp y
                            return $ prec 3 n (x' <+> text "=" <+> y')
pprProp n (Not x)     = do x' <- pprProp 2 x
                           return $ text "~" <+> x'
pprProp n (All f)     = quant "!" n f
pprProp n (Exists f)  = quant "?" n f
pprProp n TrueProp    = return $ text "$true"
pprProp n FalseProp   = return $ text "$false"

pprExp :: Exp -> Vars Doc
pprExp (Const x) = return $ text x
pprExp (Var x)   = return $ text x

binConn :: String -> Int -> Prop -> Prop -> Vars Doc
binConn op n x y = 
    do x' <- pprProp 1 x
       y' <- pprProp 1 y
       return $ prec 1 n (x' <+> text op <+> y')

quant :: String -> Int -> (Exp -> Prop) -> Vars Doc
quant q n f = 
    do x <- getUnique 
       f' <- pprProp 1 (f (Var x))
       return $ prec 1 n (text q <+> brackets (text x) <+> colon <+> f')

prec :: Int -> Int -> Doc -> Doc
prec p n = if n >= p then parens else id

--
-- * Parsing
--

instance Read Prop where
    readsPrec n = readP_to_S (readProp n)

readProp :: Int -> ReadP Prop
readProp n = skipSpaceAround $ choice rs
  where rs = [readPred, readNot, readEqual, readAll, readExists, readParen, readTrue, readFalse]
             ++ if n < 1 then [readBin] else []
        readPred = do p <- readConst
                      string "("
                      args <- sepBy readExp (string ",")
                      string ")"
                      return $ Pred p args
        readBin = do p <- readProp 1
                     op <- readBinOp
                     q <- readProp 0
                     return $ op p q
        readBinOp = choice [string "&" >> return (&&&), 
                            string "|" >> return (|||),
                            string "=>" >> return (==>),
                            string "<=>" >> return (<=>)]
        readNot = string "~" >> liftM neg (readProp 1)
        readEqual = do e1 <- readExp
                       string "="
                       e2 <- readExp
                       return $ e1 === e2
        readAll = readQuant "!" forAll
        readExists = readQuant "?" thereIs
        readQuant s q = do string s
                           skipSpaces
                           string "["
                           vars <- sepBy readVar (string ",")
                           string "]"
                           skipSpaces
                           string ":"
                           p <- readProp 1
                           return $ foldr (quantify q) p vars
        quantify q v p = q (abstract v p)
        readParen = between (string "(") (string ")") (readProp 0)
        readTrue = string "$true" >> return TrueProp
        readFalse = string "$false" >> return FalseProp

readExp :: ReadP Exp
readExp = liftM Var readVar +++ liftM Const readConst

readVar :: ReadP Var
readVar = skipSpaceAround $ liftM2 (:) (satisfy isUpper) (munch isAlphaNum)

readConst :: ReadP String
readConst = skipSpaceAround $ liftM2 (:) (satisfy isLower) (munch isAlphaNum)

skipSpaceAround :: ReadP a -> ReadP a
skipSpaceAround = between skipSpaces skipSpaces

abstract :: Var -> Prop -> (Exp -> Prop)
abstract v p = \x -> substProp v x p

--
-- * Substitution
--

substProp :: String -- ^ variable name
          -> Exp -- ^ expression to use instead
          -> Prop -> Prop
substProp v x t = 
    case t of
      Pred s es -> Pred s (map (substExp v x) es)
      And ps    -> And (map (substProp v x) ps)
      Or ps     -> Or (map (substProp v x) ps)
      Imp p q   -> Imp (substProp v x p) (substProp v x q)
      Equiv p q -> Equiv (substProp v x p) (substProp v x q)
      Not p     -> Not (substProp v x p)
      Equal e f -> Equal (substExp v x e) (substExp v x f)
      All b     -> All (\y -> substProp v x (b y))
      Exists b  -> Exists (\y -> substProp v x (b y))
      TrueProp  -> TrueProp
      FalseProp -> FalseProp

substExp :: String -- ^ variable name
         -> Exp -- ^ expression to use instead
         -> Exp -> Exp
substExp v x (Var v') | v' == v = x
substExp _ _ e = e
