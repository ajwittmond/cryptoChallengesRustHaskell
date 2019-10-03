module Main where

import Break.Utils
import System.IO
import System.Environment
import Data.String
import Data.Word
import qualified Data.ByteString as B

main :: IO ()
main = do
  string <- head <$> getArgs
  putStrLn $ toHex (B.pack $ (fromAscii "ICE") `blockXor` (fromAscii string))
