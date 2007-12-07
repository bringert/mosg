module FraCaS where

import Data.Char
import Data.Maybe

import Text.XML.HaXml.Escape
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types
import Text.XML.HaXml.Xml2Haskell

import qualified FraCaSProblemsDTD as DTD


type Sentence = String

data Answer = Yes | No | Unknown | Undef
              deriving (Show)

data Problem = Problem {
                        problemId :: Int,
                        problemPremises :: [Sentence],
                        problemQuestion :: Sentence,
                        problemAnswer :: Answer
                       }
    deriving (Show)

type FraCaS = [Problem]

readFraCaS :: FilePath -> IO FraCaS
readFraCaS = fmap toFraCaS . fReadXmlUnEscaped 

toFraCaS :: DTD.Fracas_problems -> FraCaS
toFraCaS (DTD.Fracas_problems (NonEmpty ps)) = catMaybes [toProblem p | DTD.Fracas_problems_Problem p <- ps]

toProblem :: DTD.Problem -> Maybe Problem
toProblem (DTD.Problem (DTD.Problem_Attrs { DTD.problemId = ids, 
                                            DTD.problemFracas_answer = Just a }) 
                       (NonEmpty ps) (Just (DTD.Q q)) _ _ _ _) 
    | not (null (cleanString q)) = 
    Just $ Problem { 
                    problemId = read ids,
                    problemPremises = [cleanString p | DTD.P _ p <- ps ],
                    problemQuestion = cleanString q,
                    problemAnswer = toAnswer a
                   }
toProblem _ = Nothing 

toAnswer :: DTD.Problem_fracas_answer -> Answer
toAnswer DTD.Problem_fracas_answer_yes = Yes
toAnswer DTD.Problem_fracas_answer_no = No
toAnswer DTD.Problem_fracas_answer_unknown = Unknown
toAnswer DTD.Problem_fracas_answer_undef = Undef

cleanString :: String -> String
cleanString = reverse . dropWhile isSpace . reverse . dropWhile isSpace

--
-- * Utilities
--

fReadXmlUnEscaped :: XmlContent a => FilePath -> IO a
fReadXmlUnEscaped file = 
    do x <- readFile file
       case xmlParse' file x of
         Left err -> fail err
         Right (Document _ _ y _) ->
             case fromElem [CElem (xmlUnEscape stdXmlEscaper y)] of
               (Just z,_)  -> return z
               (Nothing,_) -> fail $ file ++ ": fromElem failed"

--
-- * Debugging
--

dumpFraCaS :: IO ()
dumpFraCaS = do ps <- readFraCaS "fracas.xml"
                mapM_ (putStrLn . dumpProblem) ps
  where dumpProblem p = unlines $ [show (problemId p)] ++ problemPremises p ++ [problemQuestion p]