module TestToy where

import Toy
import FOL

import PGF

import Data.List

testParse :: String -> IO [S]
testParse s = do pgf <- readPGF "examples/toy/Toy.pgf"
                 return $ parseS s

parseS :: PGF -> String -> [S]
parseS pgf = map fg . parse pgf "ToyEng" "S"

test :: (S -> [Prop]) -> String -> IO ()
test f s = do ts <- testParse s
              putStrLn "Text:"
              putStrLn s
              putStrLn "Abstract syntax trees:"
              mapM_ print ts
              putStrLn "Formulas:"
              let fs = concatMap f ts
              mapM_ print fs
              putStrLn $ "(count: " ++ show (length fs) ++ ")"
              putStrLn "Unique formulas:"
              let unique = nub fs
              mapM_ print unique
              putStrLn $ "(count: " ++ show (length unique) ++ ")"
