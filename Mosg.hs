module Mosg (Theory, Result(..), Output(..), Answer(..), Input(..), Quest(..), Grammar, 
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

data Result = Result {
      resInputText :: String,
      resInterpretations :: Either Input [(GText,[Input])],
      resDifferentInterpretations :: [Input],
      resInputType :: InputType,
      resConsistent :: [Prop],
      resConsistentInformative :: [Prop],
      resOutput :: Output
    }

data InputType = StatementType | QuestionType | AmbiguousType

data Output = NoParse
            | NoInterpretation
            | NoConsistent
            | NoInformative
            | Ambiguous
            | AcceptedStatement Prop
            | YNQAnswer Answer
            | WhAnswer [[String]]
            | CountAnswer Int
  deriving Eq

instance Show Output where
    show (AcceptedStatement _) = "Accepted statement."
    show NoParse = "Unable to parse input."
    show NoInterpretation = "Unable to interpret input."
    show NoConsistent = "No consistent interpretation found."
    show NoInformative = "No informative interpretation found."
    show Ambiguous = "The input is ambiguous."
    show (YNQAnswer r) = "The answer is: " ++ show r
    show (WhAnswer ss) = "The answer is: " ++ show ss
    show (CountAnswer n) = "The answer is: " ++ show n

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

parseInput :: Grammar -> String -> IO (Either Input [GText])
parseInput gr i = 
    case readInputMaybe i `mplus` fmap Statement (readPropMaybe i) of
      Just input -> do debug $ "Formula input: " ++ show input
                       return $ Left input
      Nothing    -> do ps <- filterComplete $ parseText gr i
                       debug $ "Parse results: " ++ show (length ps)
                       return $ Right ps

interpretTrees :: Either Input [GText] -> IO (Either Input [(GText,[Input])])
interpretTrees (Left i) = return (Left i)
interpretTrees (Right ts) = liftM (Right . zip ts) $ mapM (filterComplete . retrieveInput . iText) ts

handleText :: Grammar -> Theory -> String -> IO Result
handleText gr th i = 
    do debug $ "Input: " ++ show i
       trees <- parseInput gr i
       treesAndInterpretations <- interpretTrees trees
       let is = either (:[]) (concatMap snd) treesAndInterpretations
       debug $ "Interpretations: " ++ show (length is)
       let is' = sortNub is
       debug $ "Syntactically different interpretations: " ++ show (length is')
       -- debug $ unlines $ map show is'
       let ss = [p | Statement p <- is']
           qs = [q | Question q  <- is']
       debug $ "Statement interpretations: " ++ show (length ss)
       debug $ "Question interpretations: " ++ show (length qs)
       let typ = case (ss,qs) of
                   (_:_,_:_) -> AmbiguousType
                   (_  , []) -> StatementType
                   ([] ,_  ) -> QuestionType
       (consistent, informative) <- 
           case typ of
             StatementType -> do sc <- filterM (liftM (==Yes) . isConsistent th) ss
                                 debug $ "Consistent statements: " ++ show (length sc)
                                 si <- filterM (liftM (==Yes) . isInformative th) sc
                                 debug $ "Consistent and informative statements: " ++ show (length si)
                                 debug $ unlines $ map show si
                                 return (sc,si)
             _             -> return ([],[])
       output <- 
           case typ of
             _ | either (const False) null trees -> return NoParse
               | null is' -> return NoInterpretation
             AmbiguousType -> return Ambiguous
             StatementType 
                 | null consistent  -> return NoConsistent
                 | null informative -> return NoInformative
                 | otherwise        -> liftM AcceptedStatement (ambiguousStatement informative)
             QuestionType  -> answerQuestion th qs
       return $ Result {
                    resInputText = i,
                    resInterpretations = treesAndInterpretations,
                    resDifferentInterpretations = is',
                    resInputType = typ,
                    resConsistent = consistent,
                    resConsistentInformative = informative,
                    resOutput = output
                  }

answerQuestion :: Theory -> [Quest] -> IO Output
answerQuestion th qs = 
    do debug $ unlines $ map show qs
       let ynq = [p | YNQuest p <- qs]
           whq = [p | WhQuest p <- qs]
           cnt = [p | CountQuest p <- qs]
       case (ynq,whq,cnt) of
         (_,[],[])   -> do q <- ambiguousQuestion ynq
                           answer <- isTrue th q
                           return (YNQAnswer answer)
         ([],[q],[]) -> do answer <- answerWhQuest th q
                           return (WhAnswer answer)
         ([],[],[q]) -> do answer <- answerWhQuest th q
                           return (CountAnswer (length (nub answer)))

ambiguousStatement :: [Prop] -> IO Prop
ambiguousStatement [p] = return p
ambiguousStatement ps = 
    do debug $ "Ambiguous statement, using disjunction."
       return $ ors ps

ambiguousQuestion :: [Prop] -> IO Prop
ambiguousQuestion [p] = return p
ambiguousQuestion ps =     
    do debug $ "Ambiguous question, using conjunction."
       return $ ands ps

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

sortNub :: Ord a => [a] -> [a]
sortNub = map head . group . sort