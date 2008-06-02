module Mosg (Theory, Mode(..), Result(..), Output(..), Answer(..), Input(..), Grammar, 
             loadGrammar, grammarModificationTime, handleText) where

import Syntax
import FOL
import ReasonFolkung
import Input
import Sem
import PGF
import Otter

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Prelude hiding (catch)
import System.Directory
import System.IO
import System.Time

data Mode = Pessimistic | Optimistic
  deriving (Show, Read, Eq, Ord)

type Grammar = PGF

type Error = String

data Result = Result {
      resInputText :: String,
      resInterpretations :: Either Input [(GText,Either Error [Input])],
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
grammarFile = "Syntax.pgf"

loadGrammar :: IO Grammar
loadGrammar = readPGF grammarFile

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

tryInput :: Show a => a -> IO (Either Error a)
tryInput p = try (evaluate (length (show p) `seq` p)) 
                 >>= either (\e -> hPutStrLn stderr (show e) >> return (Left (show e))) (return . Right)

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
      Nothing    -> do ps <- liftM catRights $ mapM tryInput $ parseText gr i
                       debug $ "Parse results: " ++ show (length ps)
                       return $ Right ps

interpretTrees :: Either Input [GText] -> IO (Either Input [(GText,Either Error [Input])])
interpretTrees (Left i) = return (Left i)
interpretTrees (Right ts) = liftM (Right . zip ts) $ mapM (tryInput . iText) ts

filterConsistent :: Theory -> [Prop] -> IO [Prop]
filterConsistent th = filterM (\p -> debug ("Checking consistency: " ++ show p) >> liftM (==Yes) (isConsistent th p))

filterInformative :: Theory -> [Prop] -> IO [Prop]
filterInformative th = filterM (\p -> debug ("Checking informativity: " ++ show p) >> liftM (==Yes) (isInformative th p))

handleText :: Mode -> Grammar -> Theory -> String -> IO Result
handleText mode gr th i = 
    do debug $ "Input: " ++ show i
       trees <- parseInput gr i
       treesAndInterpretations <- interpretTrees trees
       let is = either (:[]) (concatMap (either (const []) id . snd)) treesAndInterpretations
       debug $ "Interpretations: " ++ show (length is)
       let is' = sortNub is
       debug $ "Syntactically different interpretations: " ++ show (length is')
       -- debug $ unlines $ map show is'
       let ss = [p | Statement p <- is']
           qs = [q | q  <- is', not (isStatement q)]
       debug $ "Statement interpretations: " ++ show (length ss)
       debug $ "Question interpretations: " ++ show (length qs)
       let typ = case (ss,qs) of
                   (_:_,_:_) -> AmbiguousType
                   (_  , []) -> StatementType
                   ([] ,_  ) -> QuestionType
       (consistent, informative) <- 
           case typ of
             StatementType -> do sc <- filterConsistent th ss
                                 debug $ "Consistent statements: " ++ show (length sc)
                                 si <- filterInformative th sc
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
                 | otherwise        -> liftM AcceptedStatement (ambiguousStatement mode informative)
             QuestionType  -> answerQuestion mode th qs
       return $ Result {
                    resInputText = i,
                    resInterpretations = treesAndInterpretations,
                    resDifferentInterpretations = is',
                    resInputType = typ,
                    resConsistent = consistent,
                    resConsistentInformative = informative,
                    resOutput = output
                  }

answerQuestion :: Mode -> Theory -> [Input] -> IO Output
answerQuestion mode th qs = 
    do debug $ unlines $ map show qs
       let ynq = [p | YNQuest p <- qs]
           whq = [p | WhQuest p <- qs]
           cnt = [p | CountQuest p <- qs]
       case (ynq,whq,cnt) of
         (_,[],[])   -> do q <- ambiguousQuestion mode ynq
                           answer <- isTrue th q
                           return (YNQAnswer answer)
         ([],[q],[]) -> do answer <- answerWhQuest th q
                           return (WhAnswer answer)
         ([],[],[q]) -> do answer <- answerWhQuest th q
                           return (CountAnswer (length (nub answer)))

ambiguousStatement :: Mode -> [Prop] -> IO Prop
ambiguousStatement _ [p] = return p
ambiguousStatement Pessimistic ps = 
    do debug $ "Ambiguous statement, pessimistically using disjunction."
       return $ ors ps
ambiguousStatement Optimistic ps = 
    do debug $ "Ambiguous statement, optimistically using conjunction."
       return $ ands ps

ambiguousQuestion :: Mode -> [Prop] -> IO Prop
ambiguousQuestion _ [p] = return p
ambiguousQuestion Pessimistic ps =     
    do debug $ "Ambiguous question, pessimistically using conjunction."
       return $ ands ps
ambiguousQuestion Optimistic ps =     
    do debug $ "Ambiguous question, optimistically using disjunction."
       return $ ors ps

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

catRights :: [Either a b] -> [b]
catRights xs = [x | Right x <- xs]