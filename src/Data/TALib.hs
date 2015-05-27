module Data.TALib where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.IO.Unsafe
import Data.List.Split
import qualified Data.Vector as V

-- TODO: C functions return an enum, not a CInt

-- type signatures for this library's interface start with TS (for time-series)
-- some of these differ in naming from the TA signatures above, since the output is always the same
-- (so the suffix in type signature names above do not appear below)

-- A TSFun is a C ta-lib function, and the corresponding inputs (input data and options)
data TSFun = TA1IntInt1 CTA1IntInt1 [Double] Int Int
           | TA1Int1 CTA1Int1 [Double] Int
           | TA2_1 CTA2_1 [Double] [Double]
           | TA4IntInt1 CTA4IntInt1 [Double] [Double] [Double] [Double] Int Int
           | TA2Int2 CTA2Int2 [Double] [Double] Int
           | TA4Int1 CTA4Int1 [Double] [Double] [Double] [Double] Int
           | TA3Int1 CTA3Int1 [Double] [Double] [Double] Int
           | TA4_1 CTA4_1 [Double] [Double] [Double] [Double]
           | TA1_1 CTA1_1 [Double]
           | TA3_1 CTA3_1 [Double] [Double] [Double]
             
-- TaOutput includes the same output information provided by ta-lib:
--   outBegIdx
--   outNBElement
--   one or more lists of doubles in out, corresponding to e.g., outReal[], outAroonDown[], etc.)
data TaOutput = TaOutput { outBegIdx :: Int
                         , outNBElement :: Int
                         , outCount :: Int -- count of lists in out
                         , out :: [[Double]]
                         } deriving (Show)

taIntDefault = fromIntegral (minBound :: CInt)

ta_lib :: TSFun -> IO (Either Int TaOutput)
ta_lib tsfun
    = withArray inReal            $ \cInReal ->
      alloca                      $ \cOutBegIdx ->
      alloca                      $ \cOutNbElement ->
      allocaArray (len * outputs) $ \cOutReal ->
      -- given consecutive arrays with `n' elements, starting at Ptr `start', get the pointer to the i^th array
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- case tsfun of
          TA1IntInt1 fn _ arg1 arg2       -> fn startIdx endIdx cInReal (fromIntegral arg1) (fromIntegral arg2)
                                             cOutBegIdx cOutNbElement cOutReal
          TA1Int1 fn _ arg1               -> fn startIdx endIdx cInReal (fromIntegral arg1)
                                             cOutBegIdx cOutNbElement cOutReal
          TA2_1 fn _ _                    -> fn startIdx endIdx cInReal (getInArrPtr 1) cOutBegIdx cOutNbElement
                                             cOutReal
          TA4IntInt1 fn _ _ _ _ arg1 arg2 -> fn startIdx endIdx cInReal (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3)
                                             (fromIntegral arg1) (fromIntegral arg2) cOutBegIdx cOutNbElement
                                             cOutReal
          TA2Int2 fn _ _ arg1             -> fn startIdx endIdx cInReal (getInArrPtr 1) (fromIntegral arg1) cOutBegIdx
                                             cOutNbElement cOutReal (getOutArrPtr 1)
          TA4Int1 fn _ _ _ _ arg1         -> fn startIdx endIdx cInReal (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3)
                                             (fromIntegral arg1) cOutBegIdx cOutNbElement cOutReal
          TA3Int1 fn _ _ _ arg1           -> fn startIdx endIdx cInReal (getInArrPtr 1) (getInArrPtr 2)
                                             (fromIntegral arg1) cOutBegIdx cOutNbElement cOutReal
          TA4_1 fn _ _ _ _                -> fn startIdx endIdx cInReal (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3)
                                             cOutBegIdx cOutNbElement cOutReal
          TA1_1 fn _                      -> fn startIdx endIdx cInReal cOutBegIdx cOutNbElement cOutReal
          TA3_1 fn _ _ _                  -> fn startIdx endIdx cInReal (getInArrPtr 1) (getInArrPtr 2) cOutBegIdx
                                             cOutNbElement cOutReal
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len (map realToFrac outReal)
                                         }
          _ -> return $ Left $ fromIntegral rc
    where inReal = map realToFrac $ case tsfun of
            TA1IntInt1 _ in1 _ _             -> in1
            TA1Int1 _ in1 _                  -> in1
            TA2_1 _ in1 in2                  -> in1 ++ in2
            TA4IntInt1 _ in1 in2 in3 in4 _ _ -> in1 ++ in2 ++ in3 ++ in4
            TA2Int2 _ in1 in2 _              -> in1 ++ in2
            TA4Int1 _ in1 in2 in3 in4 _      -> in1 ++ in2 ++ in3 ++ in4
            TA3Int1 _ in1 in2 in3 _          -> in1 ++ in2 ++ in3
            TA4_1 _ in1 in2 in3 in4          -> in1 ++ in2 ++ in3 ++ in4
            TA1_1 _ in1                      -> in1
            TA3_1 _ in1 in2 in3              -> in1 ++ in2 ++ in3
          len = fromIntegral $ length $ case tsfun of
            TA1IntInt1 _ in1 _ _       -> in1
            TA1Int1 _ in1 _            -> in1
            TA2_1 _ in1 _              -> in1
            TA4IntInt1 _ in1 _ _ _ _ _ -> in1
            TA2Int2 _ in1 _ _          -> in1
            TA4Int1 _ in1 _ _ _ _      -> in1
            TA3Int1 _ in1 _ _ _        -> in1
            TA4_1 _ in1 _ _ _          -> in1
            TA1_1 _ in1                -> in1
            TA3_1 _ in1 _ _            -> in1
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = case tsfun of
            TA1IntInt1 {} -> 1
            TA1Int1 {}    -> 1
            TA2_1 {}      -> 1
            TA4IntInt1 {} -> 1
            TA2Int2 {}    -> 2
            TA4Int1 {}    -> 1
            TA3Int1 {}    -> 1
            TA4_1 {}      -> 1
            TA1_1 {}      -> 1
            TA3_1 {}      -> 1

-- TA-Lib : Technical Analysis Library

foreign import ccall unsafe "ta_common.h TA_Initialize"
  c_ta_init :: IO ()

-- // Start

-- RSI                  Relative Strength Index

foreign import ccall unsafe "ta_func.h TA_RSI"
  c_ta_rsi :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_rsi :: [Double] -> Int -> IO (Either Int TaOutput)
ta_rsi inReal optInTimePeriod
    = ta_lib (TA1Int1 c_ta_rsi inReal optInTimePeriod)

-- // End

terpri :: IO ()
terpri = putStrLn ""

main :: IO ()
main = do
  	c_ta_init
        
        let close = [91.500, 94.815, 94.375,
                     95.095, 93.780, 94.625,
                     92.530, 92.750, 90.315,
                     92.470, 96.125, 97.250,
                     98.500, 89.875, 91.000,
                     92.815, 89.155, 89.345,
                     91.625, 89.875, 88.375,
                     87.625, 84.780, 83.000]
        
        putStrLn "close"
        print close
        terpri
        
        -- TODO: pass real data into some of these, rather than passing the same series multiple times
        -- (as is, with unrealistic series, these are just testing that the C calls work and don't e.g. seg fault)
        
        let inReal = close
        let inHigh = map (1 +) close
        let inLow = map (2 +) close
        let inClose = map (2 *) close
        let inVolume = map (fromIntegral . round . (4*)) close
        
        {-
        
        putStrLn "Chaikin A/D Line"
        result <- ta_ad inHigh inLow inClose inVolume
        print result
        terpri
        
        putStrLn "Chaikin A/D Oscillator"
        result <- ta_adosc inHigh inLow inClose inVolume 6 7
        print result
        terpri
        
        putStrLn "Average Directional Movement Index"
        result <- ta_adx inHigh inLow inClose 6
        print result
        terpri
        
        putStrLn "Aroon"
        result <- ta_aroon inHigh inLow 5
        print result
        terpri
        
        putStrLn "Average True Range"
        result <- ta_atr inHigh inLow inClose 14
        print result
        terpri
        
        putStrLn "All Moving Average"
        result <- ta_ma inReal 5 taIntDefault
        print result
        terpri
        
        putStrLn "Median Price"
        result <- ta_medprice inHigh inLow
        print result
        terpri
          
        putStrLn "Money Flow Index"
        result <- ta_mfi inHigh inLow inClose inVolume 7
        print result
        terpri
        
        putStrLn "On Balance Volume"
        result <- ta_obv inReal inVolume
        print result
        terpri

        -}
        
        putStrLn "Relative Strength Index"
        result <- ta_rsi inReal 9
        print result
        terpri
        
        
        
        
        
        
        
        