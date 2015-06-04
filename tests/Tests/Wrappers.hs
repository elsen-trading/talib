module Tests.Wrappers where

import Test.Tasty
import Test.Tasty.HUnit
import Test.QuickCheck

import qualified Data.Vector.Storable as V
import Foreign.C.Types

import Data.TALib

-- ground truth numbers in tests are from the output of ta-lib-test.c

-- Apple stock prices. April 1, 2015 through April 30, 2015.
  
open = [124.82, 125.03, 124.47, 127.64, 125.85,
        125.85, 125.95, 128.37, 127.00, 126.41,
        126.28, 125.55, 125.57, 128.10, 126.99,
        128.30, 130.49, 132.31, 134.46, 130.16,
        128.64] :: [CDouble]
    
high = [125.12, 125.56, 127.51, 128.12, 126.40,
        126.58, 127.21, 128.57, 127.29, 127.13,
        127.10, 126.14, 128.12, 128.20, 128.87,
        130.42, 130.63, 133.13, 134.54, 131.59,
        128.64] :: [CDouble]
        
low = [123.10, 124.19, 124.33, 125.98, 124.97,
       124.66, 125.26, 126.61, 125.91, 126.01,
       126.11, 124.46, 125.17, 126.67, 126.32,
       128.14, 129.23, 131.15, 129.57, 128.30,
       124.58] :: [CDouble]
      
close = [124.25, 125.32, 127.35, 126.01, 125.60,
         126.56, 127.10, 126.85, 126.30, 126.78,
         126.17, 124.75, 127.60, 126.91, 128.62,
         129.67, 130.28, 132.65, 130.56, 128.64,
         125.15] :: [CDouble]
        
volume = [40621400, 32220100, 37194000, 35012300, 37329200,
          32484000, 40188000, 36365100, 25524600, 28970400,
          28369000, 51957000, 47054300, 32435100, 37654500,
          45770900, 44525900, 96954200, 118924000, 63386100,
          83195400] :: [CDouble]

vOpen = V.fromList(open)
vHigh = V.fromList(high)
vLow = V.fromList(low)
vClose = V.fromList(close)
vVolume = V.fromList(volume)

hangingMan :: Assertion
hangingMan = do
    result <- ta_cdlhangingman vOpen vHigh vLow vClose
    case result of
      Left msg -> assertFailure $ show msg
      Right (cOutBegIdx, cOutNbElement, output) -> do
        assert (cOutBegIdx == 11)
        assert (cOutNbElement == 10)
        assert $ (V.take cOutNbElement output) == (V.fromList [0,0,0,0,0,-100,0,0,0,0])

relativeStrengthIndex :: Assertion
relativeStrengthIndex = do
    result <- ta_rsi vClose 9
    case result of
      Left msg -> assertFailure $ show msg
      Right (cOutBegIdx, cOutNbElement, output) -> do
        assert (cOutBegIdx == 9)
        assert (cOutNbElement == 12)
        assert $ (V.take cOutNbElement output) == (V.fromList [66.57929226736569,
                                                               61.085224710656874,
                                                               50.2302011556407,
                                                               64.48158903377053,
                                                               59.81640942816646,
                                                               66.56138602140982,
                                                               70.03578954321449,
                                                               71.94122299230735,
                                                               78.04386475566865,
                                                               64.19272755776055,
                                                               54.243258629875314,
                                                               41.18854745239274])

-- floating point issues versus the numbers you output from C cause tests to fail with
-- a direct comparison, so just make sure the vectors are very close
simpleMovingAverage :: Assertion
simpleMovingAverage = do
    result <- ta_sma vClose 8
    case result of
      Left msg -> assertFailure $ show msg
      Right (cOutBegIdx, cOutNbElement, output) -> do
        assert (cOutBegIdx == 7)
        assert (cOutNbElement == 14)  
        V.zipWithM_ (\e1 e2 -> do
                        assert (abs (e1 - e2) < 1.0e-13))
          (V.take cOutNbElement output)
          (V.fromList [126.130000, 126.386250, 126.568750, 126.421250,
                       126.263750, 126.513750, 126.557500, 126.747500,
                       127.100000, 127.597500, 128.331250, 128.880000,
                       129.366250, 129.060000])

wrapperTests :: TestTree
wrapperTests = testGroup "Wrappers" [
  testCase "Hanging Man" hangingMan,
  testCase "Relative StrengthIndex" relativeStrengthIndex,
  testCase "Simple Moving Average" simpleMovingAverage
  ]
