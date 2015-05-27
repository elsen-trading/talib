module Tests.Exponential where

import Control.Applicative ((<$>))
import Test.Tasty
import Test.Tasty.QuickCheck

import Data.TALib.Timeseries
import Data.TALib.Extras.ExponentialMovingAverage

expAvgTests :: TestTree
expAvgTests = testGroup "Exponential Moving Averages" [
    testProperty "Exp moving average preserves data size" emaKeepsSize
  , testProperty "Exp moving average doesn't change series sum too much"
    emaKeepsSum
  ]

------------------------------------------------------------------------------
emaKeepsSize :: TauSI -> Timeseries Double -> Bool
emaKeepsSize tau ts = (length . tsSamps) (expMovAvg (kernel tau (tsFreq ts)))
                      ==
                      (length . tsSamps) ts

------------------------------------------------------------------------------
emaKeepsSum :: TauSI -> Timeseries Double -> Bool
emaKeepsSum tau ts = (Vec.sum . tsSamps) (expMovAvg (kernel tau (tsFreq ts)))
                     ==
                     (Vec.sum . tsSamps) ts
