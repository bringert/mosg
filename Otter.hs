module Otter where

import FOL

import Control.Monad
import Data.List
import System.Cmd
import System.IO
import Text.PrettyPrint.HughesPJ hiding (char)
import Text.Regex.Posix

otterPath = "/Users/bringert/bin/otter"

--
-- * Calling otter
--

answerWhQuest :: Theory -> (Ind -> Prop) -> IO [[String]]
answerWhQuest th q = liftM getAnswers $ callOtter (showWhQuest th q)

callOtter :: String -- ^ input 
          -> IO String
callOtter input =
    do writeFile "input.p" input
       system $ otterPath ++ " < input.p > output.p"
       output <- readFile "output.p"
       hPutStrLn stderr output
       return output

getAnswers :: String -> [[String]]
getAnswers s = map proofAnswers (s =~ ("^----*> .* ----*> [[:digit:]]+ \\[.*\\] (" ++ ans ++ "([[:space:]]*\\|[[:space:]]*" ++ ans ++ ")*)"))
 where
   ans = "-\\$answer\\(([[:alnum:]$]+)\\)"
   proofAnswers :: [String] -> [String]
   proofAnswers (_:a:_) = [x | (_:x:_) <- a =~ ans ]

showWhQuest :: Theory -> (Ind -> Prop) -> String
showWhQuest t q = unlines $ otterHeader ++ th ++ [q'] ++ otterFooter
  where th = map showFormula t
        q' = showFormula (neg (thereIs (\x -> q x &&& Pred "$answer" [x])))

showFormula :: Prop -> String
showFormula p = render (runVars (pprPropOtter 0 p) <> text ".")

otterHeader :: [String]
otterHeader = ["set(auto).",
               "set(prolog_style_variables).",
               "clear(print_proofs).",
               "clear(print_kept).",
               "assign(max_seconds,5).",
               "assign(max_proofs,-1).",
               "formula_list(usable)."]

otterFooter :: [String]
otterFooter = ["end_of_list."]


--
-- * Pretty-printing with Otter syntax
--

showPropOtter :: Prop -> String
showPropOtter = render . runVars . pprPropOtter 0

pprPropOtter :: Int -> Prop -> Vars Doc
pprPropOtter _ (Pred x xs) = do xs' <- mapM pprInd xs
                                return $ text x <> parens (hcat (punctuate (text ",") xs'))
pprPropOtter n (And xs)    = liftM (prec 1 n . hsep . intersperse (text "&")) $ mapM (pprPropOtter 1) xs
pprPropOtter n (Or xs)     = liftM (prec 1 n . hsep . intersperse (text "|")) $ mapM (pprPropOtter 1) xs
pprPropOtter n (Imp x y)   = binConn "->" n x y
pprPropOtter n (Equiv x y) = binConn "<->" n x y
pprPropOtter n (Equal x y) = do x' <- pprInd x
                                y' <- pprInd y
                                return $ prec 3 n (x' <+> text "=" <+> y')
pprPropOtter n (Not x)     = do x' <- pprPropOtter 2 x
                                return $ text "-" <+> x'
pprPropOtter n (All f)     = quant "all" n f
pprPropOtter n (Exists f)  = quant "exists" n f
pprPropOtter n TrueProp    = return $ text "$T"
pprPropOtter n FalseProp   = return $ text "$F"

pprInd :: Ind -> Vars Doc
pprInd (Const x) = return $ text x
pprInd (Var x)   = return $ text x

binConn :: String -> Int -> Prop -> Prop -> Vars Doc
binConn op n x y = 
    do x' <- pprPropOtter 1 x
       y' <- pprPropOtter 1 y
       return $ prec 1 n (x' <+> text op <+> y')

quant :: String -> Int -> (Ind -> Prop) -> Vars Doc
quant q n f = 
    do x <- getUnique 
       f' <- pprPropOtter 3 (f (Var x))
       return $ prec 1 n (text q <+> (text x) <+> f')

prec :: Int -> Int -> Doc -> Doc
prec p n = if n >= p then parens else id