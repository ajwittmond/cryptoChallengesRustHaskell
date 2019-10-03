module Main where

import Break.Utils
import Break.Vigenere
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
  print "Enter string to test"
  string <-  B.unpack . hexToBytes <$> getLine
  let matches = bestMatch (fl2) englishLetterFrequency ( map pure [0..255]) [string]
  print "here is the best match"
  writeFile "/home/alexanderwittmond/code/cryptoChallengesRustHaskell/matchex.txt" $
       unlines $ map (toAscii . snd) matches
  -- mapM (print . snd) matches
  print $ head $ filter (all (\c -> c >= ' ' && c <='~')) $ map (toAscii.snd) $ matches
  return ()
  
