module Main where

import BinUtils
import Data.Word
import qualified Data.IntMap as M
import Data.List.Split
import qualified Data.ByteString as B
import Debug.Trace

csvPath = "/home/alexanderwittmond/code/cryptoChallengesRustHaskell/englishLetterFrequency.csv"

stringsPath = "/home/alexanderwittmond/code/cryptoChallengesRustHaskell/4.txt"

main :: IO ()
main  = do
  englishLetterFrequency <- loadFrequencyCSV csvPath
  strings <-map (toString . hexToBytes) .  lines <$> readFile stringsPath
  let matches = bestMatch fl2 englishLetterFrequency (map (B.pack.pure) [0..255]) strings
  mapM print $ take 10 $ filter (all (\c -> fromEnum c <= 128)) $ map snd matches
  return ()
  
