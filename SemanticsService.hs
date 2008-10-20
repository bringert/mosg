
import Input
import Syntax
import Sem

import FastCGIUtils

import PGF

import Network.FastCGI
import Text.JSON

import Control.Exception
import Control.Concurrent

main :: IO ()
main = do runFastCGIConcurrent' forkIO 100 (handleErrors (handleCGIErrors cgiMain))

cgiMain :: CGI CGIResult
cgiMain =
    do tree <- getTree
       i <- liftIO $ interpret tree
       json <- case i of
                 Left u       -> return $ toJSObject [("error",showJSON ("Missing case in " ++ unhandledFunction u ++ ": " ++ showTree (unhandledSubtree u)))]
                 Right inputs -> return $ toJSObject [("interpretations", showJSON inputs)]
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
    readJSON = fmap read . readJSON -- FIXME: fail the JSON way
    showJSON = showJSON . show

tryInput :: Show a => a -> IO (Either UnhandledTree a)
tryInput p = tryUnhandledTree (evaluate (length (show p) `seq` p)) 
                 >>= either (\e -> return (Left e)) (return . Right)

interpret :: Tree -> IO (Either UnhandledTree [Input])
interpret = tryInput . interpretText . fg
