module TestToy where

import Toy
import FOL

import PGF

test :: (GS -> [Prop]) -> String -> IO ()
test f s = do pgf <- readPGF "examples/toy/Toy.pgf"
              let ts = parse pgf "ToyEng" "S" s
              mapM_ (handleTree f . fg) ts

handleTree :: (GS -> [Prop]) -> GS -> IO ()
handleTree f s = do print s
                    let fs = f s
                    mapM_ print fs
