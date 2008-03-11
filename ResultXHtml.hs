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
                 +++ body << [h1 << "MOSG Output", 
                              h2 << "Overall statistics",
                              statsHtml (statistics rs), 
                              h2 << "Problems",
                              toHtml rs]

statsHtml :: [(String,[Problem])] -> Html
statsHtml ls = table << (colgroup << map (\c -> col ! [identifier c] << noHtml) ["stats_col_text","stats_col_problems"]
                         +++ map statsLine ls)
  where statsLine (s,xs) = tr << [td << s, td << intersperse (toHtml " ") (probs xs)]
        probs xs = [anchor ! [href ("#" ++ pid)] << pid | pid <- nub $ map problemId xs]

instance HTML ProblemResult where
    toHtml r = tbody << 
               [tr ! [theclass "problem_result", identifier ("result_" ++ pid)] 
                  << [expandCell, problemIdCell, problemAnswerCell, answerCell],
                tr ! [classes ["problem_details", "expandable"], identifier ("details_" ++ pid)] 
                  << td ! [colspan 3] << details]
      where
        pid = problemId (problem r)
        expandCell = td << showHide ("details_"++pid)
        problemIdCell = td ! [theclass "problem_id"] << [anchor ! [name pid] << pid]
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
                      expandable (rid ++ "_trees") 
                                 (show countTrees ++ " parse results")
                                 (either (unordList . (:[]) . toHtml) (nonEmptyDefList . zipWith inter [1..]) (resInterpretations x)),
                      inters "unique" "different interpretations" (resDifferentInterpretations x),
                      inters "consistent" "consistent statement interpretations" (resConsistent x),
                      inters "informative" "consistent and informative statement interpretations" (resConsistentInformative x)
                     ]
            where
              rid = "problem_" ++ pid ++ "_" ++ show i
              countTrees = either (const 1) length (resInterpretations x)
              countUnique = length (resDifferentInterpretations x)
              inters i s is = expandable (rid ++ "_" ++ i) 
                                         (show (length is) ++ " " ++ s)
                                         (nonEmptyList is)
              inter _ (t,Left err) = (toHtml t, toHtml err)
              inter n (t,Right is) = (toHtml t, inters ("tree_"++show n) "Interpretations" is)

    toHtmlFromList rs = table ! [theclass "results"] 
                        << (colgroup << map (\c -> col ! [identifier c] << noHtml) ["results_col_details","results_col_id","results_col_correct","results_col_answer"]
                            +++ thead << tr << map (th <<) ["Details","ID","Correct answer","Answer"]
                            +++ map toHtml rs)


instance HTML GText where
    toHtml t = toHtml $ show t

expandable :: (HTML a, HTML b) => String -> a -> b -> Html
expandable id x y = p << [toHtml x, if isNoHtml y' then noHtml else " " +++ showHide id]
                    +++ y' ! [identifier id, theclass "expandable"]
  where y' = toHtml y

showHide :: String -> Html
showHide i = anchor ! [theclass "show_hide", href "#", strAttr "onclick" ("return toggle(this,'"++ i++"')")] << "(+)"


-- XHtml utilities

cssLink :: URL -> Html
cssLink url = 
    thelink ! [href url, rel "stylesheet", thetype "text/css"] << noHtml

javascriptLink :: URL -> Html
javascriptLink s = tag "script" ! [src s, thetype "text/javascript"] << noHtml

classes :: [String] -> HtmlAttr
classes = theclass . unwords

nonEmptyList :: HTML a => [a] -> Html
nonEmptyList = nonEmptyTag unordList 

nonEmptyDefList :: (HTML a, HTML b) => [(a,b)] -> Html
nonEmptyDefList ds = if null ds' then noHtml else defList ds' 
    where ds' = [(t',d') | (t,d) <- ds, let t' = toHtml t,
                                        let d' = toHtml d,
                                        not (isNoHtml t' && isNoHtml d')]

nonEmptyTag :: HTML a => (a -> Html) -> a -> Html
nonEmptyTag t x = if isNoHtml (toHtml x) then noHtml else t x
