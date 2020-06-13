module Main where

import Break.Detect
import System.Random
import Control.Monad.State

plainText = "Fuck The Police"

main :: IO ()
main = do
  gen <- getStdGen
  print $ results gen
  where
    results gen = map fst $ take 20 $ tail $ iterate getResults (False,gen)
    getResults (_,g) =
      let ((_,mode),gout) = aesBlackBox g $ map (fromIntegral.fromEnum) plainText
      in  (detectMode 16  (fst . fst . aesBlackBox g) == mode,gout)
