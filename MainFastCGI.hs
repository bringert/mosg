import Mosg
import MosgCGI

import System.IO
import Network.FastCGI

main :: IO ()
main = do gr <- loadGrammar
          runFastCGI (handleErrors (cgiMain gr))
