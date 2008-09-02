module TestToy where

import Toy
import FOL

import PGF

testParse :: String -> IO [GS]
testParse s = do pgf <- readPGF "examples/toy/Toy.pgf"
                 return $ map fg $ parse pgf "ToyEng" "S" s

test :: (GS -> [Prop]) -> String -> IO ()
test f s = do ts <- testParse s
              mapM_ print ts
              mapM_ (handleTree f) ts

handleTree :: (GS -> [Prop]) -> GS -> IO ()
handleTree f s = do print s
                    let fs = f s
                    mapM_ print fs
