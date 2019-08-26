module Main where


import System.Environment
import BinUtils

main :: IO ()
main = do
  args <- getArgs
  print $ toBase64 $ hexToBytes $ head args
