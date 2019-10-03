module Break.Vigenere(
 distance,
 getFrequency,
 l2,
 lp,
 Frequency(..),
 loadFrequencyCSV,
 fl2,
 flp,
 zipFrequencies,
 bestMatch,
 bestKeySize,
 breakVigenere) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Word
import Data.List
import Break.Utils
import System.IO
import Control.Arrow
import Data.String
import Data.Char
import qualified Data.IntMap.Strict as M
import Data.List.Split
import Debug.Trace
import Data.Maybe
import Control.Lens

-- represents normalized frequency
type Frequency = M.IntMap Double

distance :: Frequency -> Frequency -> Frequency
distance = M.unionWith (-)

getFrequency :: [Word8] -> Frequency
getFrequency ls =
  M.map ((/ (fromIntegral $ length ls)). fromIntegral) $
  foldl' (\m c -> M.insertWith (+) (fromEnum c) 1 m) M.empty $
  map (fromIntegral) ls

zipFrequencies :: Frequency -> Frequency -> [(Double,Double)]
zipFrequencies a b =
  let al = sort $ M.toList a
      bl = sort $ M.toList b
      doZip []          []          = []
      doZip ((a,av):as) []          = (av,0):doZip as []
      doZip []          ((b,bv):bs) = (0,bv):doZip bs []
      doZip ((a,av):as) ((b,bv):bs)
            | a==b = (av,bv):doZip as      bs
            | a<b  = (av,0):doZip (as)   ((b,bv):bs)
            | a>b  = (0,bv):doZip ((a,av):as) (bs)
  in doZip al bl

l2 :: (Foldable f, Floating a) => f (a,a) -> a
l2 = sqrt . foldl' (\acc (a,b) -> acc + (a-b)^2) 0

lp :: (Foldable f, Floating a) => a -> f (a,a) -> a
lp p = (**( 1/p)) . foldl' (\acc (a,b) -> acc + (abs (a-b))**p) 0

fl2 :: Frequency -> Frequency -> Double
fl2 a b = l2 $ zipFrequencies a b

flp :: Double -> Frequency -> Frequency -> Double
flp p a b = lp p $ zipFrequencies a b

loadFrequencyCSV :: FilePath -> IO Frequency
loadFrequencyCSV path = do
  f <- T.pack <$> readFile path
  return $ M.fromList $
    map (second ((/100.0).read) . first (fromEnum . (read :: String->Char)) .
      (\[a,b] -> (a,b)) . (\ l-> concat (intersperse "," $ take (length l-1) l) :drop (length l-1) l) .
                  map T.unpack . T.split (==',')) $
          T.lines f

bestMatch :: (Frequency -> Frequency -> Double)
              -> Frequency
              -> [[Word8]]
              -> [[Word8]]
              -> [([Word8],[Word8])]
bestMatch norm freq tests strings =   map snd $ sort $ concatMap
            (\s -> map (\key ->
              let xored = blockXor key s
              in  (norm (getFrequency xored) freq,(key,xored))
            ) tests) strings


bestKeySize:: [Int] -> [Word8] -> [Int]
bestKeySize tests string =
  map snd $ sort pairs
  where pairs = map toPair tests
        toPair i =
          ((average $ dists)/fromIntegral i, i)
          where average s = fromIntegral (sum s) / fromIntegral ( length s) :: Double
                editDist [a,b] = fromIntegral (hammingDist a b)
                editDist [a] = fromIntegral (hammingDist a $ cycle [0])
                dists = map editDist $ chunksOf 2 $ chunksOf i string

breakVigenere :: (Frequency -> Frequency -> Double) -> Frequency -> [Int] -> [Word8] -> [([Word8],[Word8])]
breakVigenere norm freq tests string =
  let  keysizes =  bestKeySize tests string
       keys = map (\keysize ->
                     let  blocks = transpose (chunksOf keysize string)
                          solveBlock block = head $ head $ map fst $
                              bestMatch norm freq (map pure [0..255]) [block]
                      in  map solveBlock blocks
                        ) keysizes
  in  map (flip blockXor string) keys `zip` keys
