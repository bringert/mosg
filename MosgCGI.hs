module MosgCGI (cgiMain) where

import Mosg
import Input_XHtml

import Control.Monad.Trans
import Network.CGI
import System.IO
import Text.XHtml

cgiMain :: Grammar -> CGI CGIResult
cgiMain gr = 
    do mit <- getInput "input_text"
       thi <- getMultiInput "theory"
       let th = map read thi
       setHeader "Content-type" "text/html; charset=UTF-8"
       case mit of
         Nothing -> outputBody $ inputPage th
         Just it -> do res <- liftIO $ handleText gr th it
                       let th' = case resOutput res of
                                   AcceptedStatement p -> th ++ [p]
                                   _ -> th
                       outputBody $ answerPage th' it res

inputPage :: Theory -> Html
inputPage th = inputForm th Nothing

answerPage :: Theory -> String -> Results -> Html
answerPage th q res = toHtml
  [answerSection q res,
   inputForm th alts]
  where alts = case resOutput res of
                 Ambiguous -> Just (resDifferentInterpretations res)
                 _         -> Nothing

answerSection :: String -> Results -> Html
answerSection q res = toHtml
  [h2 << "Answer",
   p << ("You said: " +++ quote << q),
   p << show (resOutput res)]

inputForm :: Theory -> Maybe [Input] -> Html
inputForm th mis = form ! [method "post"] << fs
  where fs = [h2 << "Input",
              fields, 
              submit "" "Submit",
              knowledgeSection th]
        fields = maybe textInput selectInput mis
        textInput = textfield "input_text" ! [size "60"] +++ focus "input_text"
        selectInput is = ordList [label << [radio "input_text" (show i), toHtml i] | i <- is ]

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
