module Main where

import System.IO
import Break.Utils
import Break.Vigenere
import qualified Data.ByteString as B
import Data.Char
import Data.List
import Data.String

filePath = "../../6.txt"
freqPath = "../../englishFreq2.csv"

main :: IO ()
main = do
  file <- filter (/= '\n') <$> readFile filePath
  englishFreq <- loadFrequencyCSV freqPath
  let bytes =  B.unpack  $ base64ToBytes file
  putStrLn "Best Guesses :"
  let best = take 5 $ breakVigenere (flp 1) englishFreq [1..40] bytes
  flip mapM_ best $ \(result,key) -> do
    putStrLn $ take 40 $ cycle "*"
    putStrLn $ "Key: "++toAscii key
    putStrLn $ toAscii result
    putStrLn $ take 40 $ cycle "*"
