module Tests.MovingAverage where

import Control.Applicative ((<$>))
import Data.Time
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Data.Vector.Storable as Vec
import Test.QuickCheck
import Test.HUnit

import Data.TALib.Timeseries
import Data.TALib.MovingAverage

movingAvgTests :: TestTree
movingAvgTests = testGroup "Moving Averages" [
  testProperty "Moving average preserves data size" maKeepsSize
  ]

maKeepsSize :: MAOpt -> Timeseries Double -> Bool
maKeepsSize opt ts =
  let len = Vec.length . tsSamps
  in  len ts == len (movingAverage opt ts)

------------------------------------------------------------------------------
instance (Arbitrary a, Vec.Storable a) => Arbitrary (Timeseries a) where
  arbitrary = let t0 = UTCTime (fromGregorian 2015 5 1) 0
                  fq = perMinute 1
              in  (\xs -> Timeseries (Vec.fromList . take 1000 $ xs) t0 fq)
                  <$> arbitrary

-- TODO: Forgot, how to properly limit the size of testcase data?
instance Arbitrary MAOpt where
  arbitrary = MAOpt . (`mod` 1000) <$> arbitrary

