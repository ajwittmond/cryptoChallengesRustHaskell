module Main where

import Break.Utils
import Data.List
import Data.List.Split
import qualified Data.ByteString as B
import Data.Bool

file = "../../8.txt"

main :: IO ()
main = do
  results <- map (countMatches . chunksOf 16 . B.unpack . hexToBytes) . lines <$> readFile file
  print $ map (\(a,b)-> (b,a)) $ reverse $ sort $ zip results [0,1..]
  where countMatches [] = 0
        countMatches (x:xs) = sum (map (bool 0 1 . (x==)) xs) + countMatches (filter (/= x) xs)
