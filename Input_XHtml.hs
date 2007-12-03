module Input_XHtml where

import Input
import FOL

import Control.Monad
import Data.Char
import Data.List
import Text.XHtml

instance HTML Input where
    toHtml = runVars . inputToHtml 0

instance HTML Quest where
    toHtml = runVars . questToHtml 0

instance HTML Prop where
    toHtml = runVars . propToHtml 0

inputToHtml :: Int -> Input -> Vars Html
inputToHtml n (Statement p) = propToHtml n p
inputToHtml n (Question q)  = questToHtml n q

questToHtml :: Int -> Quest -> Vars Html
questToHtml n (YNQuest p)    = liftM (\h -> "YNQ" +++ parens h) (propToHtml 0 p)
questToHtml n (WhQuest q)    = quant (toHtml "Which ") n q
questToHtml n (CountQuest q) = quant (toHtml "Count ") n q

propToHtml :: Int -> Prop -> Vars Html
propToHtml _ (Pred x xs) = do xs' <- mapM expToHtml xs
                              return $ x +++ parens (concatHtml (intersperse (toHtml ",") xs'))
propToHtml n (And xs)    = liftM (prec 1 n . intercalateWithSpace andHtml) $ mapM (propToHtml 1) xs
propToHtml n (Or xs)     = liftM (prec 1 n . intercalateWithSpace orHtml)  $ mapM (propToHtml 1) xs
propToHtml n (Imp x y)   = binConn impHtml n x y
propToHtml n (Equiv x y) = binConn equivHtml n x y
propToHtml n (Equal x y) = do x' <- expToHtml x
                              y' <- expToHtml y
                              return $ prec 3 n (x' <+> "=" <+> y')
propToHtml n (Not x)     = do x' <- propToHtml 2 x
                              return $ notHtml <+> x'
propToHtml n (All f)     = quant forallHtml n f
propToHtml n (Exists f)  = quant existsHtml n f
propToHtml n TrueProp    = return $ emphasize << "true"
propToHtml n FalseProp   = return $ emphasize << "false"



expToHtml :: Exp -> Vars Html
expToHtml (Const x) = return $ toHtml x
expToHtml (Var x)   = return $ var x

var :: String -> Html
var x = emphasize << map toLower x

binConn :: Html -> Int -> Prop -> Prop -> Vars Html
binConn op n x y = 
    do x' <- propToHtml 1 x
       y' <- propToHtml 1 y
       return $ prec 1 n (x' <+> op <+> y')

quant :: Html -> Int -> (Exp -> Prop) -> Vars Html
quant q n f = 
    do x <- getUnique 
       f' <- propToHtml 0 (f (Var x))
       return $ prec 1 n (q +++ var x +++ "." <+> f')

prec :: Int -> Int -> Html -> Html
prec p n = if n >= p then parens else id

parens :: Html -> Html
parens x = "(" +++ x +++ ")"

(<+>) :: (HTML a, HTML b) => a -> b -> Html
x <+> y = x +++ " " +++ y

intercalateWithSpace :: (HTML a, HTML b) => a -> [b] -> Html
intercalateWithSpace x = concatHtml . intersperse (" " +++ x +++ " ") . map toHtml

impHtml = primHtmlChar "#8658"
equivHtml = primHtmlChar "#8660"
andHtml = primHtmlChar "#8743"
orHtml = primHtmlChar "#8744"
notHtml = primHtmlChar "#172"
forallHtml = primHtmlChar "#8704"
existsHtml = primHtmlChar "#8707"
