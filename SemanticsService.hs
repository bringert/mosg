
import Input
import Syntax
import Sem

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
    do tree <- getTree
       path <- pathInfo
       json <- case filter (not . null) $ splitBy (=='/') path of
                 ["interpret"] -> doInterpret tree
                 _ -> throwCGIError 400 ("Unknown command " ++ path) ["Unknown command " ++ path ++ "."]
       outputJSONP json
  where
    getTree :: CGI Tree
    getTree = 
        do mt <- getInput "tree"
           case mt of
             Just t -> case readTree t of
                         Just tree -> return tree
                         Nothing -> throwCGIError 400 "Bad tree" ["Bad tree: " ++ show t]
             Nothing   -> throwCGIError 400 "Missing tree" ["Missing tree."]

instance JSON Input where
    showJSON (Statement p)  = showJSON $ toJSObject [("type","stm"),("prop", show p)]
    showJSON (YNQuest p)    = showJSON $ toJSObject [("type","ynquest"),("prop", show p)]
    showJSON (WhQuest u)    = let (x,p) = showFun u 
                               in showJSON $ toJSObject [("type","whquest"),("variable", x),("prop", show p)]
    showJSON (CountQuest u) = let (x,p) = showFun u 
                               in showJSON $ toJSObject [("type","countquest"),("variable", x),("prop", show p)]


doInterpret :: Tree -> CGI JSValue
doInterpret tree =
    do i <- liftIO $ interpret tree
       case i of
         Left u       -> return $ showJSON $ toJSObject [("interpretations", JSArray []),
                                              ("error", showJSON ("Missing case in " ++ unhandledFunction u ++ ": " ++ showTree (unhandledSubtree u)))]
         Right inputs -> return $ showJSON $ toJSObject [("interpretations", showJSON inputs)]


tryInput :: Show a => a -> IO (Either UnhandledTree a)
tryInput p = tryUnhandledTree (evaluate (length (show p) `seq` p)) 
                 >>= either (\e -> return (Left e)) (return . Right)

interpret :: Tree -> IO (Either UnhandledTree [Input])
interpret = tryInput . interpretText . fg
