
import FOL
import ReasonFolkung

import FastCGIUtils

import PGF

import Network.FastCGI
import Text.JSON

import Control.Exception
import Control.Concurrent
import Data.Char


main :: IO ()
main = do runFastCGIConcurrent' forkIO 100 (handleErrors (handleCGIErrors cgiMain))


cgiMain :: CGI CGIResult
cgiMain =
    do facts <- getFacts
       conjecture <- getConjecture
       path <- pathInfo
       answer <- case filter (not . null) $ splitBy (=='/') path of
                   ["istrue"]        -> liftIO $ isTrue facts conjecture
                   ["isconsistent"]  -> liftIO $ isConsistent facts conjecture
                   ["isinformative"] -> liftIO $ isInformative facts conjecture
                   _ -> throwCGIError 400 ("Unknown command " ++ path) ["Unknown command " ++ path ++ "."]
       outputJSONP $ toJSObject [("answer", showJSON answer)]
  where
    getFacts :: CGI Theory
    getFacts = getMultiInput "fact" >>= mapM readFormula

    getConjecture :: CGI Prop
    getConjecture = requiredInput "conjecture" >>= readFormula

requiredInput :: String -> CGI String
requiredInput i = 
    do m <- getInput i
       case m of
         Just x -> return x
         Nothing -> throwCGIError 400 ("Missing " ++ i) ["Missing " ++ i ++ "."]

readFormula :: String -> CGI Prop
readFormula s = 
    case reads s of
      [(p,rest)] | all isSpace rest -> return p
      _ -> throwCGIError 400 "Bad formula" ["Bad formula: " ++ show s]

instance JSON Answer where
    readJSON x = readJSON x >>= \y -> case y of
                                        "yes" -> return Yes
                                        "no" -> return No
                                        "unknown" -> return DontKnow
                                        _ -> fail $ "Bad Answer value: " ++ show y
    showJSON a = showJSON $ case a of
                              Yes      -> "yes"
                              No       -> "no"
                              DontKnow -> "unknown"
