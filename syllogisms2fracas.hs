
import Data.List
import System.IO
import Text.PrettyPrint.HughesPJ

import Text.XML.HaXml.Pretty
import Text.XML.HaXml.Xml2Haskell
import FraCaSProblemsDtd

main :: IO ()
main = do c <- readFile "report/syllogisms.txt"
          let ps = mkProblems c
          putStrLn $ showXml ps

mkProblems :: String -> Fracas_problems
mkProblems =
  Fracas_problems . NonEmpty . map Fracas_problems_Problem
  . zipWith mkProblem [1..] . filter (not . null) . splitBy null 
  . filter (not . ("#" `isPrefixOf`)) . lines . filter (/='\'')

mkProblem :: Int -> [String] -> Problem
mkProblem i ls = Problem attrs (NonEmpty (zipWith mkP [1..] ps)) Nothing (H h) Nothing Nothing (Just (Note name))
  where name = head ls
        ls' = map (drop 1) $ filter (":" `isPrefixOf`) ls
        (ps,h) = (init ls', last ls')
        attrs = Problem_Attrs (show i) (Just Problem_fracas_answer_yes) Nothing
        mkP j p = P (P_Attrs (pidx j)) p
        pidx j = [P_idx_1,P_idx_2,P_idx_3,P_idx_4,P_idx_5]!!(j-1)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy p s = case break p s of
                (l, _ : t@(_ : _)) -> l : splitBy p t
                (l, _) -> [l]