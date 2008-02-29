import Mosg
import MosgCGI

import System.IO
import Network.CGI

main :: IO ()
main = do hSetBuffering stderr LineBuffering
          gr <- loadGrammar
          runCGI (handleErrors (cgiMain gr))
