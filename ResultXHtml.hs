module ResultXHtml (statistics, writeOutput) where

import GSyntax
import Mosg
import Problem
import Input_XHtml

import Data.List
import Data.Maybe
import Data.Time
import System.Directory
import System.Locale
import System.FilePath
import Text.Printf
import Text.XHtml.Strict


statistics :: [ProblemResult] -> [(String, [Problem])]
statistics rs = 
       let goldYes          = [ problem r | r <- rs, problemAnswer (problem r) == Problem.Yes ]
           goldUnknown      = [ problem r | r <- rs, isUnknown (problemAnswer (problem r)) ]
           goldNo           = [ problem r | r <- rs, problemAnswer (problem r) == Problem.No  ]
           answered         = [ problem r | r <- rs, isJust (getAnswer r)]
           failed           = [ problem r | r <- rs, isNothing (getAnswer r)]
           correctYes       = filterAnswers rs (== Mosg.Yes)      (== Problem.Yes)
           correctUnknown   = filterAnswers rs (== Mosg.DontKnow) isUnknown
           correctNo        = filterAnswers rs (== Mosg.No)       (== Problem.No)
           incorrectYes     = filterAnswers rs (== Mosg.Yes)      (/= Problem.Yes)
           incorrectUnknown = filterAnswers rs (== Mosg.DontKnow) (not . isUnknown)
           incorrectNo      = filterAnswers rs (== Mosg.No)       (/= Problem.No)
           report' = report (length rs) 
           xs = [(problem r,res) | r <- rs, res <- premiseResults r ++ [questionResult r]]
           reportError = report (length xs)
        in [
            report' "answered"          answered,
            report' "failed"            failed,
            report' "correct yes"       correctYes,
            report' "correct no"        correctNo,
            report' "correct unknown"   correctUnknown,
            report' "incorrect yes"     incorrectYes,
            report' "incorrect no"      incorrectNo,
            report' "incorrect unknown" incorrectUnknown,
            ("",[]),
            reportError "parse errors"            [ p | (p,r) <- xs, resOutput r == NoParse],
            reportError "interpretation errors"   [ p | (p,r) <- xs, resOutput r == NoInterpretation],
            reportError "inconsistent"            [ p | (p,r) <- xs, resOutput r == NoConsistent],
            ("",[]),
            proportion "precision (yes)" correctYes (correctYes ++ incorrectYes),
            proportion "precision (no)" correctNo (correctNo ++ incorrectNo),
            proportion "recall (yes)" correctYes goldYes,
            proportion "recall (no)"  correctNo goldNo
           ]
    where
      filterAnswers rs f g = [ problem r | r <- rs, maybe False f (getAnswer r), g (problemAnswer (problem r))]

      report :: Int -> String -> [Problem] -> (String, [Problem])
      report t s xs = (printf "%5.1f%% (%3d / %3d) %s" (percentage (length xs) t) (length xs) t s, xs)

      proportion :: String -> [Problem] -> [Problem] -> (String, [Problem])
      proportion s xs ys = (printf "%5.1f%% (%3d / %3d) %s" (percentage (length xs) (length ys)) (length xs) (length ys) s, [])

      percentage :: Int -> Int -> Double
      percentage x t | t == 0 = 0
                     | otherwise = 100 * fromIntegral x / fromIntegral t


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
                 +++ body << [h1 << "MOSG Output", statsHtml (statistics rs), toHtml rs]

statsHtml :: [(String,[Problem])] -> Html
statsHtml ls = table << map statsLine ls
  where statsLine (s,xs) = tr << [td << s, td << intersperse (toHtml " ") (probs xs)]
        probs xs = [anchor ! [href ("#" ++ pid)] << pid | pid <- nub $ map problemId xs]

instance HTML ProblemResult where
    toHtml r = tbody << 
               [tr ! [theclass "problem_result", identifier ("result_" ++ pid)] 
                  << [problemIdCell, problemAnswerCell, answerCell],
                tr ! [theclass "problem_details", identifier ("details_" ++ pid)] 
                  << td ! [colspan 3] << details]
      where
        pid = problemId (problem r)
        problemIdCell = td ! [theclass "problem_id"] 
                        << [anchor ! [name pid] << pid,
                            toHtml " ",
                            showHide ("details_"++pid)]
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
        details = [ordList (zipWith result [1..] (premiseResults r ++ [questionResult r]))]

        result i x = [p << resInputText x,
                      p << (("Parse results: " ++ show countTrees) 
                            +++ if countTrees > 0 then showHide (rid ++ "_trees") else noHtml),
                      either (unordList . (:[]) . toHtml) (defList . map inter) (resInterpretations x)
                             ! [theclass "trees", identifier (rid ++ "_trees")],
                      p << (("Different interpretations: " ++ show countUnique) 
                            +++ if countUnique > 0 then showHide (rid ++ "_unique") else noHtml),
                      unordList (resDifferentInterpretations x)
                             ! [theclass "unique", identifier (rid ++ "_unique")]
                     ]
            where
              rid = pid ++ "_" ++ show i
              countTrees = either (const 1) length (resInterpretations x)
              countUnique = length (resDifferentInterpretations x)
              inter (t,Left err) = (toHtml t, toHtml err)
              inter (t,Right is) = (toHtml t, unordList is)

    toHtmlFromList rs = table ! [theclass "results"] 
                        << (thead << tr << map (th <<) ["ID","Problem answer","Answer"]
                            +++ map toHtml rs)


instance HTML GText where
    toHtml t = toHtml $ show t

showHide :: String -> Html
showHide i = anchor ! [theclass "show_hide", href "#", strAttr "onclick" ("return toggle(this,'"++ i++"')")] << "+"

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