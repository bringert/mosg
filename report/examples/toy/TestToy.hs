module TestToy where

import Toy
import FOL

import PGF

testParse :: String -> IO [S]
testParse s = do pgf <- readPGF "examples/toy/Toy.pgf"
                 return $ map fg $ parse pgf "ToyEng" "S" s

test :: (S -> [Prop]) -> String -> IO ()
test f s = do ts <- testParse s
              mapM_ print ts
              mapM_ (handleTree f) ts

handleTree :: (S -> [Prop]) -> S -> IO ()
handleTree f s = do print s
                    let fs = f s
                    mapM_ print fs
