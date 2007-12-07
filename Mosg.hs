module Mosg (Theory, Output(..), Input(..), Quest(..), Grammar, 
             loadGrammar, handleText) where

import GSyntax
import FOL
import ReasonFolkung
import Input
import Sem
import Keller
import GF.GFCC.API
import Otter

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Prelude hiding (catch)
import System.IO

type Grammar = MultiGrammar

data Output = AcceptedStatement Prop
            | NoParse
            | NoInterpretation [GUtt]
            | NoConsistent [Prop]
            | NoInformative [Prop]
            | Ambiguous [Input]
            | YNQAnswer Prop Result
            | WhAnswer (Exp -> Prop) [[String]]
            | CountAnswer (Exp -> Prop) Int

loadGrammar :: IO Grammar
loadGrammar = file2grammar "Union.gfcc"

parseUtt :: Grammar -> String -> [GUtt]
parseUtt gr = map fg . concat . parseAll gr "Utt"

tryInput :: Input -> IO (Either Exception Input)
tryInput p = try (evaluate (length (show p) `seq` p))

-- | Keep only interpretations that do not throw exceptions.
filterComplete :: [Input] -> IO [Input]
filterComplete = fmap concat . mapM (\i -> tryInput i >>= either (\e -> putStrLn (show e) >> return []) (return . (:[]))) 

readInputMaybe :: String -> Maybe Input
readInputMaybe s = case [x | (x,t) <- reads s, all isSpace t] of
                         [x] -> Just x
                         _   -> Nothing

readPropMaybe :: String -> Maybe Prop
readPropMaybe s = case [x | (x,t) <- reads s, all isSpace t] of
                         [x] -> Just x
                         _   -> Nothing

handleText :: Grammar -> Theory -> String -> IO Output
handleText gr th i = 
    do debug "-------------------------------------"
       debug "Input:"
       debug $ show i
       case readInputMaybe i `mplus` fmap Statement (readPropMaybe i) of
         Just input -> do debug $ "Formula input: "
                          debug $ show input
                          handleInputs th [input]
         Nothing    -> do let ps = parseUtt gr i
                          debug $ "Parse results: " ++ show (length ps)
                          debug $ unlines $ map show ps
                          if null ps 
                            then return NoParse
                            else handleUtts th ps

handleUtts :: Theory -> [GUtt] -> IO Output
handleUtts th ps =
   do let is = concatMap (retrieveInput . iUtt) ps
      is' <- filterComplete is
      debug $ "Interpretations: " ++ show (length is')
      debug $ unlines $ map show is'
      let is'' = nub is'
      debug $ "Syntactically different interpretations: " ++ show (length is'')
      debug $ unlines $ map show is''
      if null is'' then return (NoInterpretation ps) 
                   else handleInputs th is''

handleInputs :: Theory -> [Input] -> IO Output
handleInputs th is =
    do let ss = [p | Statement p <- is] 
           qs = [q | Question q <- is]
       debug $ "Statement interpretations: " ++ show (length ss)
       debug $ "Question interpretations: " ++ show (length qs)
       sc <- filterM (liftM (==Yes) . isConsistent th) ss
       debug $ "Consistent statements: " ++ show (length sc)
       if null sc && null qs 
         then return (NoConsistent ss)
         else do si <- filterM (liftM (==Yes) . isInformative th) sc
                 debug $ "Consistent and informative statements: " ++ show (length si)
                 if null si && null qs 
                   then return (NoInformative sc)
                   else do sd <- nubEquivalent th si
                           let is' = map Statement sd ++ map Question qs
                           debug $ "Distinct consistent and informative interpretations: "  ++ show (length is')
                           debug $ unlines $ map show is'
                           case is' of
                             [x] -> useInput th x
                             _   -> return (Ambiguous is')

useInput :: Theory -> Input -> IO Output
useInput th (Statement s) = return (AcceptedStatement s)
useInput th (Question q)  = answerQuestion th q

answerQuestion :: Theory -> Quest -> IO Output
answerQuestion th (YNQuest q) = do answer <- isTrue th q
                                   return (YNQAnswer q answer)
answerQuestion th (WhQuest q) = do answer <- answerWhQuest th q
                                   return (WhAnswer q answer)
answerQuestion th (CountQuest q) = do answer <- answerWhQuest th q
                                      return (CountAnswer q (length (nub answer)))

nubEquivalent :: Theory -> [Prop] -> IO [Prop]
nubEquivalent th = nubByM (\p q -> liftM (==Yes) $ areEquivalent th p q)

--
-- * Logging
--

debug :: String -> IO ()
debug = hPutStrLn stderr

--
-- * Utilities
--

nubByM :: Monad m => (a -> a -> m Bool) -> [a] -> m [a]
nubByM _ []     = return []
nubByM f (x:xs) = liftM (x:) $ filterM (liftM not . f x) xs >>= nubByM f
