import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.IO.Unsafe
import Data.List.Split
import qualified Data.Vector as V

-- TODO: C functions return an enum, not a CInt

-- C ta-lib type signatures start with CTA

-- CTA1IntInt1 means that there is 1 input array, an Int option, another Int option, and 1 output array
type CTA1IntInt1 = CInt        -- startIdx
                -> CInt        -- endIdx
                -> Ptr CDouble -- input array
                -> CInt        -- option
                -> CInt        -- option
                -> Ptr CInt    -- outBegIdx
                -> Ptr CInt    -- outNBElement
                -> Ptr CDouble -- output array
                -> IO CInt

-- CTA2_1 - 2 input arrays, no options, and 1 output array
type CTA2_1 = CInt        -- startIdx
           -> CInt        -- endIdx
           -> Ptr CDouble -- input array
           -> Ptr CDouble -- input array
           -> Ptr CInt    -- outBegIdx
           -> Ptr CInt    -- outNBElement
           -> Ptr CDouble -- output array
           -> IO CInt

-- CTA4IntInt1 - 4 input arrays, an Int option, another Int option, and 1 output array
type CTA4IntInt1 = CInt        -- startIdx
                -> CInt        -- endIdx
                -> Ptr CDouble -- input array
                -> Ptr CDouble -- input array
                -> Ptr CDouble -- input array
                -> Ptr CDouble -- input array
                -> CInt        -- option
                -> CInt        -- option
                -> Ptr CInt    -- outBegIdx
                -> Ptr CInt    -- outNBElement
                -> Ptr CDouble -- output array
                -> IO CInt              

-- CTA2Int2 - 2 input arrays, an Int option, and 2 output arrays
type CTA2Int2 = CInt        -- startIdx
             -> CInt        -- endIdx
             -> Ptr CDouble -- input array
             -> Ptr CDouble -- input array
             -> CInt        -- option
             -> Ptr CInt    -- outBegIdx
             -> Ptr CInt    -- outNBElement
             -> Ptr CDouble -- output array
             -> Ptr CDouble -- output array
             -> IO CInt

-- CTA4Int1 - 4 input arrays, an Int option, and 1 output array
type CTA4Int1 = CInt        -- startIdx
             -> CInt        -- endIdx
             -> Ptr CDouble -- input array
             -> Ptr CDouble -- input array
             -> Ptr CDouble -- input array
             -> Ptr CDouble -- input array
             -> CInt        -- option
             -> Ptr CInt    -- outBegIdx
             -> Ptr CInt    -- outNBElement
             -> Ptr CDouble -- output array
             -> IO CInt

-- type signatures for this library's interface start with TS (for time-series)
-- some of these differ in naming from the TA signatures above, since the output is always the same
-- (so the suffix in type signature names above do not appear below)

type TSOutput = IO (Either Int TaOutput)

type TS1IntInt = [Double]
              -> Int
              -> Int
              -> TSOutput

type TS2_ = [Double]
         -> [Double]
         -> TSOutput

type TS4IntInt = [Double]
              -> [Double]
              -> [Double]
              -> [Double]
              -> Int
              -> Int
              -> TSOutput

type TS2Int = [Double]
           -> [Double]
           -> Int
           -> TSOutput

type TS4Int = [Double]
           -> [Double]
           -> [Double]
           -> [Double]
           -> Int
           -> TSOutput


-- A TSFun is a C ta-lib function, and the corresponding inputs (input data and options)
data TSFun = TA1IntInt1 CTA1IntInt1 [Double] Int Int
           | TA2_1 CTA2_1 [Double] [Double]
           | TA4IntInt1 CTA4IntInt1 [Double] [Double] [Double] [Double] Int Int
           | TA2Int2 CTA2Int2 [Double] [Double] Int
           | TA4Int1 CTA4Int1 [Double] [Double] [Double] [Double] Int

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

ta_lib :: TSFun -> TSOutput
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
          TA2_1 fn _ _                    -> fn startIdx endIdx cInReal (getInArrPtr 1) cOutBegIdx cOutNbElement
                                             cOutReal
          TA4IntInt1 fn _ _ _ _ arg1 arg2 -> fn startIdx endIdx cInReal (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3)
                                             (fromIntegral arg1) (fromIntegral arg2) cOutBegIdx cOutNbElement
                                             cOutReal
          TA2Int2 fn _ _ arg1             -> fn startIdx endIdx cInReal (getInArrPtr 1) (fromIntegral arg1) cOutBegIdx
                                             cOutNbElement cOutReal (getOutArrPtr 1)
          TA4Int1 fn _ _ _ _ arg1         -> fn startIdx endIdx cInReal (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3)
                                             (fromIntegral arg1) cOutBegIdx cOutNbElement cOutReal
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
            TA2_1 _ in1 in2                  -> in1 ++ in2
            TA4IntInt1 _ in1 in2 in3 in4 _ _ -> in1 ++ in2 ++ in3 ++ in4
            TA2Int2 _ in1 in2 _              -> in1 ++ in2
            TA4Int1 _ in1 in2 in3 in4 _      -> in1 ++ in2 ++ in3 ++ in4
          len = fromIntegral $ length $ case tsfun of
            TA1IntInt1 _ in1 _ _       -> in1
            TA2_1 _ in1 _              -> in1
            TA4IntInt1 _ in1 _ _ _ _ _ -> in1
            TA2Int2 _ in1 _ _          -> in1
            TA4Int1 _ in1 _ _ _ _      -> in1
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = case tsfun of
            TA1IntInt1 {} -> 1
            TA2_1 {}      -> 1
            TA4IntInt1 {} -> 1
            TA2Int2 {}    -> 2
            TA4Int1 {}    -> 1

foreign import ccall unsafe "ta_common.h TA_Initialize"
  c_ta_init :: IO ()

-- input: inReal[]; options: int optInTimePeriod, int optInMAType; output: outReal[]
foreign import ccall unsafe "ta_func.h TA_MA"
  c_ta_ma :: CTA1IntInt1

taMovingAverage :: TS1IntInt
taMovingAverage inReal optInTimePeriod optInMAType
    = ta_lib (TA1IntInt1 c_ta_ma inReal optInTimePeriod optInMAType)

-- input: inHigh[], inLow[]; options: none; output: outReal[]
foreign import ccall unsafe "ta_func.h TA_MEDPRICE"
  c_ta_medprice :: CTA2_1

taMedianPrice :: TS2_
taMedianPrice inHigh inLow
    = ta_lib (TA2_1 c_ta_medprice inHigh inLow)

-- input:   inHigh[], inLow[], inClose[], inVolume[];
-- options: int optInFastPeriod (2-100000), int optInSlowPeriod (2-100000);
-- output:  outReal[]
foreign import ccall unsafe "ta_func.h TA_ADOSC"
  c_ta_adosc :: CTA4IntInt1

taChaikinAdOscillator :: TS4IntInt
taChaikinAdOscillator inHigh inLow inClose inVolume optInFastPeriod optInSlowPeriod
    = ta_lib (TA4IntInt1 c_ta_adosc inHigh inLow inClose inVolume optInFastPeriod optInSlowPeriod)

-- input:   inHigh[], inLow[];
-- options: int optInTimePeriod (2-100000);
-- output:  outAroonDown[], outAroonUp[]
foreign import ccall unsafe "ta_func.h TA_AROON"
  c_ta_aroon :: CTA2Int2

taAroon :: TS2Int
taAroon inHigh inLow optInTimePeriod
    = ta_lib (TA2Int2 c_ta_aroon inHigh inLow optInTimePeriod)

-- input: inHigh[], inLow[], inClose[], inVolume[];
-- options: int optInTimePeriod (2-100000);
-- output: outReal[]
foreign import ccall unsafe "ta_func.h TA_MFI"
  c_ta_mfi :: CTA4Int1

taMoneyFlowIndex :: TS4Int
taMoneyFlowIndex inHigh inLow inClose inVolume optInTimePeriod
    = ta_lib (TA4Int1 c_ta_mfi inHigh inLow inClose inVolume optInTimePeriod)
      
-- input: inReal[], inVolume[]; options: none; output: outReal[]
foreign import ccall unsafe "ta_func.h TA_OBV"
  c_ta_obv :: CTA2_1

taOnBalanceVolume :: TS2_
taOnBalanceVolume inReal inVolume
    = ta_lib (TA2_1 c_ta_obv inReal inVolume)

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
        
        let open = map (1 +) close
            
        -- TODO: pass real data into some of these, rather than passing the same series multiple times
        
        putStrLn "Moving Average"
        result <- taMovingAverage close 5 taIntDefault
        print result
        terpri
        
        putStrLn "Median Price"
        result <- taMedianPrice open close
        print result
        terpri
        
        putStrLn "Chaikin A/D Oscillator"
        result <- taChaikinAdOscillator open close open close 6 7
        print result
        terpri
        
        putStrLn "Aroon"
        result <- taAroon open close 5
        print result
        terpri
          
        putStrLn "Money Flow Index"
        result <- taMoneyFlowIndex open close open close 7
        print result
        terpri
                