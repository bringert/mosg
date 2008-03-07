module ResultXHtml where

import Mosg
import Problem

import Data.Time
import System.Directory
import System.Locale
import System.FilePath
import Text.Printf
import Text.XHtml.Strict


mkOutputDir :: IO FilePath
mkOutputDir = 
    do t <- getZonedTime
       let dir = formatTime defaultTimeLocale "output-%Y%m%d-%H%M%S" t
       createDirectory dir
       return dir


writeOutput :: [ProblemResult] -> IO ()
writeOutput rs = 
    do dir <- mkOutputDir
       let htmlFile = dir </> "index.html"
       copyFile "style.css" (dir </> "style.css")
       writeFile htmlFile $ renderHtml $ resultsPage rs
       printf "Output written to %s\n" htmlFile

resultsPage :: [ProblemResult] -> Html
resultsPage rs = header << [thetitle << "MOSG results", cssLink "style.css"]
                 +++ body << toHtml rs

instance HTML ProblemResult where
    toHtml r = tr ! [identifier (problemId (problem r))] 
               << [problemIdCell, problemAnswerCell, answerCell]
      where
        problemIdCell = td ! [theclass "problem_id"] << problemId (problem r)
        problemAnswerCell = td ! [theclass ("problem_answer_"++s)] << s
            where s = case problemAnswer (problem r) of
                        Problem.Yes     -> "yes"
                        Problem.No      -> "no"
                        Problem.Unknown -> "unknown"
                        Problem.Undef   -> "undefined"
        answerCell = td ! [classes ["answer_"++s,corr]] << s
            where s = case getAnswer r of
                        Just Mosg.Yes      -> "yes"
                        Just Mosg.No       -> "no"
                        Just Mosg.DontKnow -> "unknown"
                        Nothing            -> "failed"
                  corr = maybe "failed" (\c -> if c then "correct" else "incorrect") (isCorrect r)
    toHtmlFromList rs = table ! [theclass "results"] 
                        << (tr << map (th <<) ["ID","Problem answer","Answer"]
                            +++ map toHtml rs)


{-
data Result = Result {
      resInputText :: String,
      resTrees :: Either Input [GText],
      resInterpretations :: [[Input]],
      resDifferentInterpretations :: [Input],
      resInputType :: InputType,
      resConsistent :: [Prop],
      resConsistentInformative :: [Prop],
      resOutput :: Output
    }
-}

-- XHtml utilities

cssLink :: URL -> Html
cssLink url = 
    thelink ! [href url, rel "stylesheet", thetype "text/css"] << noHtml

classes :: [String] -> HtmlAttr
classes = theclass . unwords