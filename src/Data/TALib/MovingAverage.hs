{-# LANGUAGE ForeignFunctionInterface #-}

module Data.TALib.MovingAverage where

import           Data.List
import qualified Data.Vector.Storable as Vec

import Data.TALib.Timeseries

data MAOpt = MAOpt {
  maWindowLength :: Int
  } deriving (Eq, Show)

movingAverage :: MAOpt -> Timeseries Double -> Timeseries Double
movingAverage (MAOpt len) vec
  | Vec.null (unTS vec) = vec
  | len == 0            = vec
  | otherwise =
  let sampsList    = Vec.toList . unTS $ vec
      mean :: [Double] -> Double
      mean xs      = let f (s, l ) x = (s+x, succ l :: Int)
                         (sm,ln)     = foldl' f (0,0) xs
                     in sm / fromIntegral ln
      movAvg       = map mean . neighborhood len $ sampsList
  in  Timeseries . Vec.fromList $ movAvg

neighborhood :: Int -> [Double] -> [[Double]]
neighborhood n   [] = []
neighborhood len xs = case len of
  0 -> map (const []) xs
  1 -> map (: [])     xs
  n -> let buff = replicate n (head xs)
       in  tail . scanl (\neigs x -> tail neigs ++ [x]) buff $ xs

t :: Timeseries Double
t = Timeseries $ Vec.fromList [0,0,0,0,1,0,0,0.0]
