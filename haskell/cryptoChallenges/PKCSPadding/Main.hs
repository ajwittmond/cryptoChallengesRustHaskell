module Main where

import Break.Utils
import Data.String
import qualified Data.ByteString as B

str = B.unpack $ fromString "YELLOW SUBMARINE"

main :: IO ()
main = putStr $ toAscii $ pad 20 str
