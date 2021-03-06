import Mosg
import MosgCGI
import FastCGIUtils

import Data.IORef
import System.IO
import System.Time

import Network.CGI


main :: IO ()
main = do initFastCGI
          r <- newGrammarRef
          loopFastCGI (fcgiMain r)

fcgiMain :: IORef (Maybe (ClockTime, Grammar)) -> CGI CGIResult
fcgiMain r = getGrammar r >>= cgiMain


-- Reload grammar when modified

type GrammarRef = IORef (Maybe (ClockTime, Grammar))

newGrammarRef :: MonadIO m => m GrammarRef
newGrammarRef = liftIO $ newIORef Nothing

getGrammar :: MonadIO m => GrammarRef -> m Grammar
getGrammar r = liftIO $
    do t' <- grammarModificationTime
       m <- readIORef r
       case m of
         Just (t,g) | t' == t -> return g
         _                    -> do logCGI "Loading grammar..."
                                    g <- loadGrammar
                                    writeIORef r (Just (t',g))
                                    return g
