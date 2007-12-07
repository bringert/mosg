import Mosg
import Input_XHtml

import Control.Monad.Trans
import Network.CGI
import System.IO
import Text.XHtml

main :: IO ()
main = do hSetBuffering stderr LineBuffering
          runCGI (handleErrors cgiMain)

cgiMain :: CGI CGIResult
cgiMain = 
    do gr <- liftIO loadGrammar
       mit <- getInput "input_text"
       thi <- getMultiInput "theory"
       let th = map read thi
       setHeader "Content-type" "text/html; charset=UTF-8"
       case mit of
         Nothing -> outputBody $ inputPage th
         Just it -> do out <- liftIO $ handleText gr th it
                       let th' = case out of
                                   AcceptedStatement p -> th ++ [p]
                                   _ -> th
                       outputBody $ answerPage th' it out

inputPage :: Theory -> Html
inputPage th = inputForm th Nothing

answerPage :: Theory -> String -> Output -> Html
answerPage th q out = toHtml
  [answerSection q out,
   inputForm th alts]
  where alts = case out of
                 Ambiguous is -> Just is
                 _            -> Nothing

answerSection :: String -> Output -> Html
answerSection q output = toHtml
  [h2 << "Answer",
   p << ("You said: " +++ quote << q),
   answer output]
  where 
    answer (AcceptedStatement f) = p << ("Accepted " +++ f)
    answer NoParse = p << "Unable to parse input."
    answer (NoInterpretation us) = p << "Unable to interpret input."
    answer (NoConsistent ps) = p << "No consistent interpretation found."
    answer (NoInformative ps) = p << "No informative interpretation found."
    answer (Ambiguous is) = p << "The input is ambiguous."
    answer (YNQAnswer q r) = p << ("The answer to " +++ q +++ " is " +++ strong << result r)
    answer (WhAnswer q ss) = p << toHtml (WhQuest q) +++ " = " +++ show ss
    result = toHtml . show

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
