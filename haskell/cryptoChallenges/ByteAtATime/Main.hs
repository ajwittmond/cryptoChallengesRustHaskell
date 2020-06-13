{-# LANGUAGE FlexibleContexts #-}
module Main where
import Break.Utils
import qualified Data.ByteString as B
import System.Random
import Break.Detect
import Break.Vigenere
import qualified Data.Map as M
import Data.List.Split
import Data.List
import Control.Monad.State
import Data.Maybe


string = "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"

decoded = B.unpack $ base64ToBytes string


main :: IO ()
main = do
  gen <- getStdGen
  let cipher bytes = fst $ ecbEncryptor gen (bytes ++ decoded)
      keySizes =  bestKeySize [4..20] $ cipher (take 10000 $ cycle [0])
      keySize = head keySizes
      mode = detectMode keySize cipher
  putStrLn  $  "keysize: " ++ show keySize
  putStrLn $ "mode: " ++ show mode
  let stringChunks = chunksOf keySize $ B.unpack $ base64ToBytes string
  output <- concat <$> execStateT (mapM_ (decodeBlock cipher keySize) stringChunks) []
  return ()
  where decodeBlock cipher keySize chunck = do
          known <- get
          let newBlock = execState (mapM_ (decodeBlock' cipher known) [1..keySize]) []
          put $ known ++ newBlock
          return ()
          where decodeBlock' cipher known i = do
                  knownFromBlock <- get
                  let prefix = known ++ take (i-1) (cycle [0]) ++ knownFromBlock
                      dictionary = M.fromList $ map (genKeyPair prefix) [0..255]
                  put <- knownFromBlock ++ [fromJust $ M.lookup (cipher prefix) dictionary]
                  return ()
                  where genKeyPair prefix byte = (take (ceiling $ fromIntegral $ length prefix) $ cipher $ prefix ++ [byte],byte)
