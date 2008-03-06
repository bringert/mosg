module Mosg (Theory, Output(..), Result(..), Input(..), Quest(..), Grammar, 
             loadGrammar, grammarModificationTime, handleText) where

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
import System.Directory
import System.IO
import System.Time

type Grammar = MultiGrammar

data Output = AcceptedStatement Prop
            | NoParse
            | NoInterpretation [GText]
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

grammarFile :: FilePath
grammarFile = "Union.gfcc"

loadGrammar :: IO Grammar
loadGrammar = file2grammar grammarFile

grammarModificationTime :: IO ClockTime
grammarModificationTime = getModificationTime grammarFile

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

parseText :: Grammar -> String -> [GText]
parseText gr = map fg . concat . parseAll gr "Text" . preprocess

tryInput :: Show a => a -> IO (Either Exception a)
tryInput p = try (evaluate (length (show p) `seq` p))

-- | Keep only interpretations that do not throw exceptions.
filterComplete :: Show a => [a] -> IO [a]
filterComplete = fmap concat . mapM (\i -> tryInput i >>= either (\e -> hPutStrLn stderr (show e) >> return []) (return . (:[]))) 

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
         Nothing    -> do ps <- filterComplete $ parseText gr i
                          debug $ "Parse results: " ++ show (length ps)
                          -- debug $ unlines $ map (showTree . gf) ps
                          if null ps 
                            then return NoParse
                            else handleTrees th ps

interpretTexts :: [GText] -> IO [Input]
interpretTexts = 
    liftM concat . filterComplete . map (retrieveInput . iText)

handleTrees :: Theory -> [GText] -> IO Output
handleTrees th ps =
   do is <- interpretTexts ps
      debug $ "Interpretations: " ++ show (length is)
      let is' = nub is
      debug $ "Syntactically different interpretations: " ++ show (length is')
      -- debug $ unlines $ map show is'
      if null is' 
        then return $ NoInterpretation ps
        else handleInputs th is'

handleInputs :: Theory -> [Input] -> IO Output
handleInputs th is =
    do let ss = [p | Statement p <- is] 
           qs = [q | Question q <- is]
       debug $ "Statement interpretations: " ++ show (length ss)
       debug $ "Question interpretations: " ++ show (length qs)
       case (ss,qs) of
         (_:_,_:_) -> return (Ambiguous is)
         (_  , []) -> handleStatements th ss
         ([] ,_  ) -> handleQuestions th qs

handleStatements :: Theory -> [Prop] -> IO Output
handleStatements th ss = 
    do sc <- filterM (liftM (==Yes) . isConsistent th) ss
       debug $ "Consistent statements: " ++ show (length sc)
       if null sc
         then return (NoConsistent ss)
         else do si <- filterM (liftM (==Yes) . isInformative th) sc
                 debug $ "Consistent and informative statements: " ++ show (length si)
                 if null si
                   then return (NoInformative sc)
                   else do sd <- nubEquivalent th si
                           debug $ "Distinct consistent and informative statements: "  ++ show (length sd)
                           debug $ unlines $ map show sd
                           return (AcceptedStatement (ambiguousStatement sd))

handleQuestions :: Theory -> [Quest] -> IO Output
handleQuestions th qs = 
    do let ynq = [p | YNQuest p <- qs]
           whq = [p | WhQuest p <- qs]
           cnt = [p | CountQuest p <- qs]
       case (ynq,whq,cnt) of
         (_,[],[])   -> do let q = ambiguousQuestion ynq
                           answer <- isTrue th q
                           return (YNQAnswer q answer)
         ([],[q],[]) -> do answer <- answerWhQuest th q
                           return (WhAnswer q answer)
         ([],[],[q]) -> do answer <- answerWhQuest th q
                           return (CountAnswer q (length (nub answer)))

ambiguousStatement :: [Prop] -> Prop
ambiguousStatement = ors

ambiguousQuestion :: [Prop] -> Prop
ambiguousQuestion = ands

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
