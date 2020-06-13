module Main where

import Break.Utils
import Crypto.Cipher.Types hiding (cbcDecrypt,cbcEncrypt)
import Crypto.Cipher.AES hiding (cbcEncrypt, cbcDecrypt)
import Crypto.Error
import Data.List
import Data.List.Split
import Data.Word
import Data.String
import qualified Data.ByteString as B

testString = "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure."

key = fromString "YELLOW SUBMARINE" :: B.ByteString

file = "../../10.txt"

cbcEncrypt :: (BlockCipher cipher) => cipher -> [Word8] -> [Word8] -> [Word8]
cbcEncrypt cipher iv plainText =
  concat $ snd $ mapAccumL doCbc iv $ blocks
  where blocks = chunksOf (blockSize cipher) $  pad (blockSize cipher) plainText
        spread a = (a,a)
        doCbc prev next = spread $ B.unpack $ ecbEncrypt cipher $ B.pack $ blockXor prev next

cbcDecrypt :: (BlockCipher cipher) => cipher -> [Word8] -> [Word8] -> [Word8]
cbcDecrypt cipher iv plainText =
  concat $ snd $ mapAccumL doCbc iv $ blocks
  where blocks = chunksOf (blockSize cipher) $  pad (blockSize cipher) plainText
        spread a = (a,a)
        doCbc prev next = (next, blockXor prev $ B.unpack $ ecbDecrypt cipher $ B.pack next)

main :: IO ()
main = do
  let padded = pad 16 $ fromAscii testString
      iv = take 16 $ cycle [0]
  onCryptoFailure (const $ print "could not initialize cipher") (\ cipher -> do
      print $ padded == (cbcDecrypt cipher iv $ cbcEncrypt cipher iv padded)
      f <- readFile file
      putStrLn $ toAscii $ cbcDecrypt cipher iv $ B.unpack $ base64ToBytes $ filter isBase64 f
    ) (cipherInit key :: CryptoFailable AES128)
