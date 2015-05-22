module Tests.MovingAverage where

import Control.Applicative ((<$>))
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
  let len = Vec.length . unTS
  in  len ts == len (movingAverage opt ts)

-- TODO: Forgot, how to properly limit the size of testcase data?
instance Arbitrary MAOpt where
  arbitrary = MAOpt . (`mod` 1000) <$> arbitrary

instance (Arbitrary a, Vec.Storable a) => Arbitrary (Timeseries a) where
  arbitrary = Timeseries . Vec.fromList . take 1000 <$> arbitrary
