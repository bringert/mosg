module Mosg (Theory, Output(..), Result(..), Input(..), Quest(..), Grammar, 
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

instance Show Output where
    show (AcceptedStatement f) = "Accepted " ++ show f
    show NoParse = "Unable to parse input."
    show (NoInterpretation us) = "Unable to interpret input."
    show (NoConsistent ps) = "No consistent interpretation found."
    show (NoInformative ps) = "No informative interpretation found."
    show (Ambiguous is) = "The input is ambiguous."
    show (YNQAnswer q r) = "The answer to " ++ show q ++ " is " ++ show r
    show (WhAnswer q ss) = show (WhQuest q) ++ " = " ++ show ss

loadGrammar :: IO Grammar
loadGrammar = file2grammar "Union.gfcc"


preprocess :: String -> String
preprocess = unwords . unfoldr split
  where
    split :: String -> Maybe (String,String)
    split "" = Nothing
    split (c:cs) | isSpace c = split cs
                 | isPunctuation c = Just ([c], cs)
                 | isDigit c && not (null cs) && isDigit (head cs) = Just ([c], cs)
                 | otherwise = let (w,cs') = break isBreak cs in Just (c:w,cs')
        where isBreak x = isSpace x || (isPunctuation x && x `notElem` "-'")

parseUtt :: Grammar -> String -> [GUtt]
parseUtt gr = map fg . concat . parseAll gr "Utt" . preprocess

tryInput :: Show a => a -> IO (Either Exception a)
tryInput p = try (evaluate (length (show p) `seq` p))

-- | Keep only interpretations that do not throw exceptions.
filterComplete :: Show a => [a] -> IO [a]
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
    do debug $ "Input: " ++ show i
       case readInputMaybe i `mplus` fmap Statement (readPropMaybe i) of
         Just input -> do debug $ "Formula input: "
                          debug $ show input
                          handleInputs th [input]
         Nothing    -> do let ps = parseUtt gr i
                          debug $ "Parse results: " ++ show (length ps)
                          debug $ unlines $ map (showTree . gf) ps
                          if null ps 
                            then return NoParse
                            else handleUtts th ps

interpretUtts :: [GUtt] -> IO [Input]
interpretUtts = 
    liftM concat . filterComplete . map (retrieveInput . iUtt)

handleUtts :: Theory -> [GUtt] -> IO Output
handleUtts th ps =
   do is <- interpretUtts ps
      debug $ "Interpretations: " ++ show (length is)
      if null is 
        then return (NoInterpretation ps)
        else do -- debug $ unlines $ map show is
                let is' = nub is
                debug $ "Syntactically different interpretations: " ++ show (length is')
                if null is' 
                  then return (NoInterpretation ps) 
                  else do -- debug $ unlines $ map show is'
                          handleInputs th is'

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
