module Main where

import Break.Utils
import Break.Vigenere
import Data.Word
import qualified Data.IntMap as M
import Data.List.Split
import qualified Data.ByteString as B
import Debug.Trace
import Data.Char

csvPath = "/home/alexanderwittmond/code/cryptoChallengesRustHaskell/englishLetterFrequency.csv"

stringsPath = "/home/alexanderwittmond/code/cryptoChallengesRustHaskell/4.txt"

main :: IO ()
main  = do
  englishLetterFrequency <- loadFrequencyCSV csvPath
  strings <-map (B.unpack . hexToBytes) .  lines <$> readFile stringsPath
  let matches = filter (all isText) . map (toAscii.snd) $
         bestMatch (flp 1) englishLetterFrequency (map pure [0..255]) strings
  putStrLn $ head matches
  return ()
  
