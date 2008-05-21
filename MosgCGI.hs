module MosgCGI (cgiMain) where

import Mosg
import Input_XHtml

import Control.Monad.Trans
import Data.Maybe
import Network.CGI
import System.IO
import Text.XHtml

cgiMain :: Grammar -> CGI CGIResult
cgiMain gr = 
    do mode <- fmap (fromMaybe Pessimistic) $ readInput "mode"
       mit <- getInput "input_text"
       thi <- getMultiInput "theory"
       let th = map read thi
       setHeader "Content-type" "text/html; charset=UTF-8"
       case mit of
         Nothing -> outputBody $ inputPage mode th
         Just it -> do res <- liftIO $ handleText mode gr th it
                       let th' = case resOutput res of
                                   AcceptedStatement p -> th ++ [p]
                                   _ -> th
                       outputBody $ answerPage mode th' it res

inputPage :: Mode -> Theory -> Html
inputPage mode th = inputForm mode th Nothing

answerPage :: Mode -> Theory -> String -> Result -> Html
answerPage mode th q res = toHtml
  [answerSection q res,
   inputForm mode th alts]
  where alts = case resOutput res of
                 Ambiguous -> Just (resDifferentInterpretations res)
                 _         -> Nothing

answerSection :: String -> Result -> Html
answerSection q res = toHtml
  [h2 << "Answer",
   p << ("You said: " +++ quote << q),
   p << show (resOutput res)]

inputForm :: Mode -> Theory -> Maybe [Input] -> Html
inputForm defmode th mis = form ! [method "post"] << fs
  where fs = [h2 << "Input",
              fields, 
              submit "" "Submit",
              h2 << "Settings",
              mode,
              knowledgeSection th]
        fields = maybe textInput selectInput mis
        textInput = textfield "input_text" ! [size "60"] +++ focus "input_text"
        selectInput is = ordList [label << [radio "input_text" (show i), toHtml i] | i <- is ]
        mode = "Mode: " +++ [label << (radio "mode" (show m) ! (if m == defmode then [checked] else []) +++ show m)
             | m <- [Optimistic,Pessimistic]]

knowledgeSection :: Theory -> Html
knowledgeSection th = toHtml
  [h2 << "Knowledge",
   ordList [label << [checkbox "theory" (show p) ! [checked], toHtml p] | p <- th]]

outputBody :: HTML a => a -> CGI CGIResult
outputBody = outputHtml . mkPage

mkPage :: HTML a => a -> Html
mkPage b = [thetitle << "MOSG"] +++ body << b


--
-- * Utilities
--

outputHtml :: HTML a => a -> CGI CGIResult
outputHtml = output . renderHtml . toHtml

focus :: String -> Html
focus cid = inlineJavascript ("document.getElementById("++show cid++").focus();") 

inlineJavascript :: String -> Html
inlineJavascript s = tag "script" ! [thetype "text/javascript"] << primHtml s
