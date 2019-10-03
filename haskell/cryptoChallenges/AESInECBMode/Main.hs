module Main where

import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error
import Break.Utils
import Data.Char
import qualified Data.ByteString as B
import Data.String
import Data.Maybe

file = "../../7.txt"
key = fromString "YELLOW SUBMARINE" :: B.ByteString

main :: IO ()
main =
  maybe
    (do
      putStrLn "failed to create cipher"
    )
    (\cipher -> do
      s <- readFile file
      let bytes = base64ToBytes $ filter isBase64 s
      putStrLn $ toAscii $ B.unpack $ ecbDecrypt cipher bytes
    )
    (maybeCryptoError $ cipherInit key :: Maybe AES128)
  
