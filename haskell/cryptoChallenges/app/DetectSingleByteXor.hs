module DetectSingleByteXor where

import BinUtils
import Data.Word


csvPath = "/home/alexanderwittmond/code/cryptoChallengesRustHaskell/englishLetterFrequency.csv"

stringsPath = "/home/alexanderwittmond/code/cryptoChallengesRustHaskell/4.txt"

main :: IO ()
main  = do
  englishLetterFrequency <- loadFrequencyCSV csvPath
  strings <- lines <$> readFile stringsPath
  let decoded = map strings
  return ()
  
