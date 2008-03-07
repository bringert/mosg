module ResultXHtml where

import Mosg
import Problem
import Input_XHtml

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
       copyFile "ui.js" (dir </> "ui.js")
       writeFile htmlFile $ renderHtml $ resultsPage rs
       printf "Output written to %s\n" htmlFile

resultsPage :: [ProblemResult] -> Html
resultsPage rs = header << [thetitle << "MOSG results", 
                            cssLink "style.css",
                            javascriptLink "ui.js"]
                 +++ body << toHtml rs

instance HTML ProblemResult where
    toHtml r = tbody << 
               [tr ! [theclass "problem_result", identifier ("result_" ++ problemId (problem r))] 
                  << [problemIdCell, problemAnswerCell, answerCell],
                tr ! [theclass "problem_details", identifier ("details_"++problemId (problem r))] 
                  << td ! [colspan 2] << details]
      where
        problemIdCell = td ! [theclass "problem_id", rowspan 2] 
                        << [toHtml (problemId (problem r)),
                            toHtml " ",
                            showHide "+" ("details_"++problemId (problem r))]
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
        details = ordList $ map (unordList . resConsistentInformative) (premiseResults r)

    toHtmlFromList rs = table ! [theclass "results"] 
                        << (thead << tr << map (th <<) ["ID","Problem answer","Answer"]
                            +++ map toHtml rs)

showHide :: String -> String -> Html
showHide t i = anchor ! [theclass "show_hide", href ("javascript:toggle('"++ i++"')")] << t

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

javascriptLink :: URL -> Html
javascriptLink s = tag "script" ! [src s, thetype "text/javascript"] << noHtml

classes :: [String] -> HtmlAttr
classes = theclass . unwords