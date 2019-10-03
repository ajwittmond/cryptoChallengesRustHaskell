module Main where


import System.Environment
import Break.Utils

main :: IO ()
main = do
  args <- getArgs
  print $ toBase64 $ hexToBytes $ head args
