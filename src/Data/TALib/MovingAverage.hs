{- NOTE: Not a real implementation of movingAverage. Just for testing -}
{-# LANGUAGE ForeignFunctionInterface #-}

module Data.TALib.MovingAverage where

import           Control.Applicative ((<$>))
import           Data.List
import qualified Data.Vector.Storable as Vec

import Data.TALib.Timeseries

data MAOpt = MAOpt {
  maWindowNSamps :: Int
  } deriving (Eq, Show)

{- NOTE: Not a real implementation of movingAverage. Just for testing -}
movingAverage :: MAOpt -> Timeseries Double -> Timeseries Double
movingAverage (MAOpt len) vec
  | Vec.null (tsSamps vec) = vec
  | len == 0               = vec
  | otherwise              =
  let sampsList    = Vec.toList . tsSamps $ vec
      mean :: [Double] -> Double
      mean xs      = let f (s, l ) x = (s+x, succ l :: Int)
                         (sm,ln)     = foldl' f (0,0) xs
                     in sm / fromIntegral ln
      movAvg       = map mean . neighborhood len $ sampsList
  in vec {tsSamps = Vec.fromList movAvg}

neighborhood :: Int -> [Double] -> [[Double]]
neighborhood n   [] = []
neighborhood len xs = case len of
  0 -> map (const []) xs
  1 -> map (: [])     xs
  n -> let buff = replicate n (head xs)
       in  tail . scanl (\neigs x -> tail neigs ++ [x]) buff $ xs
