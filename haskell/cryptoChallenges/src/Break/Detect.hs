module Break.Detect (
  aesBlackBox,
  ecbEncryptor,
  detectMode,
  Mode(..)
) where

import Data.Word
import Data.List.Split
import Data.List
import System.Random
import Data.Tuple
import qualified Data.ByteString as B
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error
import Break.Utils
import Data.Maybe

data Mode = ECB | CBC
            deriving (Eq,Ord,Show)

aesBlackBox :: StdGen ->  [Word8] -> (([Word8],Mode),StdGen)
aesBlackBox gen string =
  ((B.unpack $ encrypt string,if useEcb then ECB else CBC), nextGen)
  where
    (useEcb,gen') = random gen
    (prefix,gen'') = randomR (5,10) gen'
    (postfix,gen''') = randomR (5,10) gen''
    (gen'''',key) = mapAccumL (\g -> const $ swap $ random g  ) gen''' [0..15]
    (nextGen,iv) = mapAccumL (\g -> const $ swap $ random g  ) gen'''' [0..15]
    cipher = throwCryptoError $ cipherInit (B.pack key) :: AES128
    encrypt s =
        if useEcb then
          ecbEncrypt cipher padded
        else
          cbcEncrypt cipher (fromJust $ makeIV $ B.pack iv) padded
      where
        bytes = cycle [4]
        padded = B.pack $ pad 16 $ take prefix bytes ++ s ++ take postfix bytes

ecbEncryptor :: StdGen ->  [Word8] -> ([Word8],StdGen)
ecbEncryptor gen' string =
  ((B.unpack $ encrypt string), nextGen)
  where
    (prefix,gen'') = randomR (5,10) gen'
    (postfix,gen''') = randomR (5,10) gen''
    (nextGen,key) = mapAccumL (\g -> const $ swap $ random g) gen''' [0..15]
    cipher = throwCryptoError $ cipherInit (B.pack key) :: AES128
    encrypt s =
          ecbEncrypt cipher padded
      where
        bytes = cycle [4]
        padded = B.pack $ pad 16 $ take prefix bytes ++ s ++ take postfix bytes

repeatedBlocks :: Int -> [Word8] -> Int
repeatedBlocks i = countRepeats . chunksOf i
  where countRepeats [] = 0
        countRepeats (x:xs) = length (filter (==x) xs) + countRepeats (filter (/=x) xs)

detectMode :: Int -> ([Word8] -> [Word8]) -> Mode
detectMode blockSize cypher =
  if repeatedBlocks blockSize (cypher $ take (100*blockSize) $ cycle [0]) > 0 then ECB else CBC
