
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
       i <- liftIO $ interpret tree
       json <- case i of
                 Left u       -> return $ toJSObject [("interpretations", JSArray []),
                                                       ("error", showJSON ("Missing case in " ++ unhandledFunction u ++ ": " ++ showTree (unhandledSubtree u)))]
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
    readJSON x = readJSON x >>= readM
    showJSON = showJSON . show

tryInput :: Show a => a -> IO (Either UnhandledTree a)
tryInput p = tryUnhandledTree (evaluate (length (show p) `seq` p)) 
                 >>= either (\e -> return (Left e)) (return . Right)

interpret :: Tree -> IO (Either UnhandledTree [Input])
interpret = tryInput . interpretText . fg


readM :: (Monad m, Read a) => String -> m a
readM s = case reads s of
            [(x,rest)] | all isSpace rest -> return x
            _ -> fail $ "read failed: " ++ show s
