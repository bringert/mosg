import PGF

import Control.Monad
import Data.List
import System.IO

pgfFile = "Union.gfcc"

inputCat = "Utt"

main :: IO ()
main = do gfp <- readPGF pgfFile
          loop gfp

loop :: PGF -> IO ()
loop pgf = 
    do eof <- isEOF
       if eof then return () 
         else do line <- getLine
                 let tokens = lexer line
                     res = parseAllLang pgf inputCat tokens
                     ls =  [l ++ ": " ++ showTree t | (l,ts) <- res, t <- ts]
                 when (not (null ls)) $ putStrLn (show (length ls) ++ ": " ++ line)
                 loop pgf

diag :: PGF -> [Token] -> IO ()
diag pgf ts = sequence_ [unless (null ws) $ putStrLn $ "Unknown words ("++l++"): " ++ show ws | l <- languages pgf, let ws = unknownWords pgf l ts]

