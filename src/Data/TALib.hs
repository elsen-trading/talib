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

taIntDefault = fromIntegral (minBound :: CInt)

-- TA-Lib : Technical Analysis Library

foreign import ccall unsafe "ta_common.h TA_Initialize"
  c_ta_init :: IO ()

-- *********************************
-- *** Start Auto-Generated Code ***
-- *********************************

--
-- ACOS                 Vector Trigonometric ACos
--

foreign import ccall unsafe "ta_func.h TA_ACOS"
  c_ta_acos :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_acos :: [Double] -> IO (Either Int (Int, Int, [Double]))
ta_acos inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_acos startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- AD                   Chaikin A/D Line
--

foreign import ccall unsafe "ta_func.h TA_AD"
  c_ta_ad :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
--   inVolume
-- arguments
-- outputs
--   outReal (double[])

ta_ad :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Double]))
ta_ad inHigh inLow inClose inVolume
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_ad startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose ++ inVolume
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- ADD                  Vector Arithmetic Add
--

foreign import ccall unsafe "ta_func.h TA_ADD"
  c_ta_add :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal0
--   inReal1
-- arguments
-- outputs
--   outReal (double[])

ta_add :: [Double] -> [Double] -> IO (Either Int (Int, Int, [Double]))
ta_add inReal0 inReal1
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_add startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal0 ++ inReal1
          len = fromIntegral $ length inReal0
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- ADOSC                Chaikin A/D Oscillator
--

foreign import ccall unsafe "ta_func.h TA_ADOSC"
  c_ta_adosc :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
--   inVolume
-- arguments
--   optInFastPeriod (int)
--   optInSlowPeriod (int)
-- outputs
--   outReal (double[])

ta_adosc :: [Double] -> [Double] -> [Double] -> [Double] -> Int -> Int -> IO (Either Int (Int, Int, [Double]))
ta_adosc inHigh inLow inClose inVolume optInFastPeriod optInSlowPeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_adosc startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) (fromIntegral optInFastPeriod) (fromIntegral optInSlowPeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose ++ inVolume
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- ADX                  Average Directional Movement Index
--

foreign import ccall unsafe "ta_func.h TA_ADX"
  c_ta_adx :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_adx :: [Double] -> [Double] -> [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_adx inHigh inLow inClose optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_adx startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- ADXR                 Average Directional Movement Index Rating
--

foreign import ccall unsafe "ta_func.h TA_ADXR"
  c_ta_adxr :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_adxr :: [Double] -> [Double] -> [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_adxr inHigh inLow inClose optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_adxr startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- APO                  Absolute Price Oscillator
--

foreign import ccall unsafe "ta_func.h TA_APO"
  c_ta_apo :: CInt -> CInt -> Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInFastPeriod (int)
--   optInSlowPeriod (int)
--   optInMAType (int)
-- outputs
--   outReal (double[])

ta_apo :: [Double] -> Int -> Int -> Int -> IO (Either Int (Int, Int, [Double]))
ta_apo inReal optInFastPeriod optInSlowPeriod optInMAType
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_apo startIdx endIdx (getInArrPtr 0) (fromIntegral optInFastPeriod) (fromIntegral optInSlowPeriod) (fromIntegral optInMAType) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- AROON                Aroon
--

foreign import ccall unsafe "ta_func.h TA_AROON"
  c_ta_aroon :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outAroonDown (double[])
--   outAroonUp (double[])

ta_aroon :: [Double] -> [Double] -> Int -> IO (Either Int (Int, Int, [Double], [Double]))
ta_aroon inHigh inLow optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_aroon startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0,
                                 chunks !! 1
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 2

--
-- AROONOSC             Aroon Oscillator
--

foreign import ccall unsafe "ta_func.h TA_AROONOSC"
  c_ta_aroonosc :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_aroonosc :: [Double] -> [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_aroonosc inHigh inLow optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_aroonosc startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- ASIN                 Vector Trigonometric ASin
--

foreign import ccall unsafe "ta_func.h TA_ASIN"
  c_ta_asin :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_asin :: [Double] -> IO (Either Int (Int, Int, [Double]))
ta_asin inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_asin startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- ATAN                 Vector Trigonometric ATan
--

foreign import ccall unsafe "ta_func.h TA_ATAN"
  c_ta_atan :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_atan :: [Double] -> IO (Either Int (Int, Int, [Double]))
ta_atan inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_atan startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- ATR                  Average True Range
--

foreign import ccall unsafe "ta_func.h TA_ATR"
  c_ta_atr :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_atr :: [Double] -> [Double] -> [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_atr inHigh inLow inClose optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_atr startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- AVGPRICE             Average Price
--

foreign import ccall unsafe "ta_func.h TA_AVGPRICE"
  c_ta_avgprice :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outReal (double[])

ta_avgprice :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Double]))
ta_avgprice inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_avgprice startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- BBANDS               Bollinger Bands
--

foreign import ccall unsafe "ta_func.h TA_BBANDS"
  c_ta_bbands :: CInt -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
--   optInNbDevUp (double)
--   optInNbDevDn (double)
--   optInMAType (int)
-- outputs
--   outRealUpperBand (double[])
--   outRealMiddleBand (double[])
--   outRealLowerBand (double[])

ta_bbands :: [Double] -> Int -> Double -> Double -> Int -> IO (Either Int (Int, Int, [Double], [Double], [Double]))
ta_bbands inReal optInTimePeriod optInNbDevUp optInNbDevDn optInMAType
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_bbands startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) (realToFrac optInNbDevUp) (realToFrac optInNbDevDn) (fromIntegral optInMAType) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1) (getOutArrPtr 2)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0,
                                 chunks !! 1,
                                 chunks !! 2
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 3

--
-- BETA                 Beta
--

foreign import ccall unsafe "ta_func.h TA_BETA"
  c_ta_beta :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal0
--   inReal1
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_beta :: [Double] -> [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_beta inReal0 inReal1 optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_beta startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal0 ++ inReal1
          len = fromIntegral $ length inReal0
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- BOP                  Balance Of Power
--

foreign import ccall unsafe "ta_func.h TA_BOP"
  c_ta_bop :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outReal (double[])

ta_bop :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Double]))
ta_bop inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_bop startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CCI                  Commodity Channel Index
--

foreign import ccall unsafe "ta_func.h TA_CCI"
  c_ta_cci :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_cci :: [Double] -> [Double] -> [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_cci inHigh inLow inClose optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cci startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDL2CROWS            Two Crows
--

foreign import ccall unsafe "ta_func.h TA_CDL2CROWS"
  c_ta_cdl2crows :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdl2crows :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdl2crows inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdl2crows startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDL3BLACKCROWS       Three Black Crows
--

foreign import ccall unsafe "ta_func.h TA_CDL3BLACKCROWS"
  c_ta_cdl3blackcrows :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdl3blackcrows :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdl3blackcrows inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdl3blackcrows startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDL3INSIDE           Three Inside Up/Down
--

foreign import ccall unsafe "ta_func.h TA_CDL3INSIDE"
  c_ta_cdl3inside :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdl3inside :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdl3inside inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdl3inside startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDL3LINESTRIKE       Three-Line Strike 
--

foreign import ccall unsafe "ta_func.h TA_CDL3LINESTRIKE"
  c_ta_cdl3linestrike :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdl3linestrike :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdl3linestrike inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdl3linestrike startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDL3OUTSIDE          Three Outside Up/Down
--

foreign import ccall unsafe "ta_func.h TA_CDL3OUTSIDE"
  c_ta_cdl3outside :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdl3outside :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdl3outside inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdl3outside startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDL3STARSINSOUTH     Three Stars In The South
--

foreign import ccall unsafe "ta_func.h TA_CDL3STARSINSOUTH"
  c_ta_cdl3starsinsouth :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdl3starsinsouth :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdl3starsinsouth inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdl3starsinsouth startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDL3WHITESOLDIERS    Three Advancing White Soldiers
--

foreign import ccall unsafe "ta_func.h TA_CDL3WHITESOLDIERS"
  c_ta_cdl3whitesoldiers :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdl3whitesoldiers :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdl3whitesoldiers inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdl3whitesoldiers startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLABANDONEDBABY     Abandoned Baby
--

foreign import ccall unsafe "ta_func.h TA_CDLABANDONEDBABY"
  c_ta_cdlabandonedbaby :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInPenetration (double)
-- outputs
--   outInteger (int[])

ta_cdlabandonedbaby :: [Double] -> [Double] -> [Double] -> [Double] -> Double -> IO (Either Int (Int, Int, [Int]))
ta_cdlabandonedbaby inOpen inHigh inLow inClose optInPenetration
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlabandonedbaby startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) (realToFrac optInPenetration) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLADVANCEBLOCK      Advance Block
--

foreign import ccall unsafe "ta_func.h TA_CDLADVANCEBLOCK"
  c_ta_cdladvanceblock :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdladvanceblock :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdladvanceblock inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdladvanceblock startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLBELTHOLD          Belt-hold
--

foreign import ccall unsafe "ta_func.h TA_CDLBELTHOLD"
  c_ta_cdlbelthold :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlbelthold :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlbelthold inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlbelthold startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLBREAKAWAY         Breakaway
--

foreign import ccall unsafe "ta_func.h TA_CDLBREAKAWAY"
  c_ta_cdlbreakaway :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlbreakaway :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlbreakaway inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlbreakaway startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLCLOSINGMARUBOZU   Closing Marubozu
--

foreign import ccall unsafe "ta_func.h TA_CDLCLOSINGMARUBOZU"
  c_ta_cdlclosingmarubozu :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlclosingmarubozu :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlclosingmarubozu inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlclosingmarubozu startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLCONCEALBABYSWALL  Concealing Baby Swallow
--

foreign import ccall unsafe "ta_func.h TA_CDLCONCEALBABYSWALL"
  c_ta_cdlconcealbabyswall :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlconcealbabyswall :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlconcealbabyswall inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlconcealbabyswall startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLCOUNTERATTACK     Counterattack
--

foreign import ccall unsafe "ta_func.h TA_CDLCOUNTERATTACK"
  c_ta_cdlcounterattack :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlcounterattack :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlcounterattack inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlcounterattack startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLDARKCLOUDCOVER    Dark Cloud Cover
--

foreign import ccall unsafe "ta_func.h TA_CDLDARKCLOUDCOVER"
  c_ta_cdldarkcloudcover :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInPenetration (double)
-- outputs
--   outInteger (int[])

ta_cdldarkcloudcover :: [Double] -> [Double] -> [Double] -> [Double] -> Double -> IO (Either Int (Int, Int, [Int]))
ta_cdldarkcloudcover inOpen inHigh inLow inClose optInPenetration
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdldarkcloudcover startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) (realToFrac optInPenetration) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLDOJI              Doji
--

foreign import ccall unsafe "ta_func.h TA_CDLDOJI"
  c_ta_cdldoji :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdldoji :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdldoji inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdldoji startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLDOJISTAR          Doji Star
--

foreign import ccall unsafe "ta_func.h TA_CDLDOJISTAR"
  c_ta_cdldojistar :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdldojistar :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdldojistar inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdldojistar startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLDRAGONFLYDOJI     Dragonfly Doji
--

foreign import ccall unsafe "ta_func.h TA_CDLDRAGONFLYDOJI"
  c_ta_cdldragonflydoji :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdldragonflydoji :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdldragonflydoji inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdldragonflydoji startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLENGULFING         Engulfing Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLENGULFING"
  c_ta_cdlengulfing :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlengulfing :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlengulfing inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlengulfing startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLEVENINGDOJISTAR   Evening Doji Star
--

foreign import ccall unsafe "ta_func.h TA_CDLEVENINGDOJISTAR"
  c_ta_cdleveningdojistar :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInPenetration (double)
-- outputs
--   outInteger (int[])

ta_cdleveningdojistar :: [Double] -> [Double] -> [Double] -> [Double] -> Double -> IO (Either Int (Int, Int, [Int]))
ta_cdleveningdojistar inOpen inHigh inLow inClose optInPenetration
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdleveningdojistar startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) (realToFrac optInPenetration) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLEVENINGSTAR       Evening Star
--

foreign import ccall unsafe "ta_func.h TA_CDLEVENINGSTAR"
  c_ta_cdleveningstar :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInPenetration (double)
-- outputs
--   outInteger (int[])

ta_cdleveningstar :: [Double] -> [Double] -> [Double] -> [Double] -> Double -> IO (Either Int (Int, Int, [Int]))
ta_cdleveningstar inOpen inHigh inLow inClose optInPenetration
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdleveningstar startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) (realToFrac optInPenetration) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLGAPSIDESIDEWHITE  Up/Down-gap side-by-side white lines
--

foreign import ccall unsafe "ta_func.h TA_CDLGAPSIDESIDEWHITE"
  c_ta_cdlgapsidesidewhite :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlgapsidesidewhite :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlgapsidesidewhite inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlgapsidesidewhite startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLGRAVESTONEDOJI    Gravestone Doji
--

foreign import ccall unsafe "ta_func.h TA_CDLGRAVESTONEDOJI"
  c_ta_cdlgravestonedoji :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlgravestonedoji :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlgravestonedoji inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlgravestonedoji startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLHAMMER            Hammer
--

foreign import ccall unsafe "ta_func.h TA_CDLHAMMER"
  c_ta_cdlhammer :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlhammer :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlhammer inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlhammer startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLHANGINGMAN        Hanging Man
--

foreign import ccall unsafe "ta_func.h TA_CDLHANGINGMAN"
  c_ta_cdlhangingman :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlhangingman :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlhangingman inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlhangingman startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLHARAMI            Harami Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLHARAMI"
  c_ta_cdlharami :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlharami :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlharami inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlharami startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLHARAMICROSS       Harami Cross Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLHARAMICROSS"
  c_ta_cdlharamicross :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlharamicross :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlharamicross inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlharamicross startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLHIGHWAVE          High-Wave Candle
--

foreign import ccall unsafe "ta_func.h TA_CDLHIGHWAVE"
  c_ta_cdlhighwave :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlhighwave :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlhighwave inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlhighwave startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLHIKKAKE           Hikkake Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLHIKKAKE"
  c_ta_cdlhikkake :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlhikkake :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlhikkake inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlhikkake startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLHIKKAKEMOD        Modified Hikkake Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLHIKKAKEMOD"
  c_ta_cdlhikkakemod :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlhikkakemod :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlhikkakemod inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlhikkakemod startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLHOMINGPIGEON      Homing Pigeon
--

foreign import ccall unsafe "ta_func.h TA_CDLHOMINGPIGEON"
  c_ta_cdlhomingpigeon :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlhomingpigeon :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlhomingpigeon inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlhomingpigeon startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLIDENTICAL3CROWS   Identical Three Crows
--

foreign import ccall unsafe "ta_func.h TA_CDLIDENTICAL3CROWS"
  c_ta_cdlidentical3crows :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlidentical3crows :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlidentical3crows inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlidentical3crows startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLINNECK            In-Neck Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLINNECK"
  c_ta_cdlinneck :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlinneck :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlinneck inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlinneck startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLINVERTEDHAMMER    Inverted Hammer
--

foreign import ccall unsafe "ta_func.h TA_CDLINVERTEDHAMMER"
  c_ta_cdlinvertedhammer :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlinvertedhammer :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlinvertedhammer inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlinvertedhammer startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLKICKING           Kicking
--

foreign import ccall unsafe "ta_func.h TA_CDLKICKING"
  c_ta_cdlkicking :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlkicking :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlkicking inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlkicking startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLKICKINGBYLENGTH   Kicking - bull/bear determined by the longer marubozu
--

foreign import ccall unsafe "ta_func.h TA_CDLKICKINGBYLENGTH"
  c_ta_cdlkickingbylength :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlkickingbylength :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlkickingbylength inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlkickingbylength startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLLADDERBOTTOM      Ladder Bottom
--

foreign import ccall unsafe "ta_func.h TA_CDLLADDERBOTTOM"
  c_ta_cdlladderbottom :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlladderbottom :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlladderbottom inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlladderbottom startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLLONGLEGGEDDOJI    Long Legged Doji
--

foreign import ccall unsafe "ta_func.h TA_CDLLONGLEGGEDDOJI"
  c_ta_cdllongleggeddoji :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdllongleggeddoji :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdllongleggeddoji inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdllongleggeddoji startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLLONGLINE          Long Line Candle
--

foreign import ccall unsafe "ta_func.h TA_CDLLONGLINE"
  c_ta_cdllongline :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdllongline :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdllongline inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdllongline startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLMARUBOZU          Marubozu
--

foreign import ccall unsafe "ta_func.h TA_CDLMARUBOZU"
  c_ta_cdlmarubozu :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlmarubozu :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlmarubozu inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlmarubozu startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLMATCHINGLOW       Matching Low
--

foreign import ccall unsafe "ta_func.h TA_CDLMATCHINGLOW"
  c_ta_cdlmatchinglow :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlmatchinglow :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlmatchinglow inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlmatchinglow startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLMATHOLD           Mat Hold
--

foreign import ccall unsafe "ta_func.h TA_CDLMATHOLD"
  c_ta_cdlmathold :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInPenetration (double)
-- outputs
--   outInteger (int[])

ta_cdlmathold :: [Double] -> [Double] -> [Double] -> [Double] -> Double -> IO (Either Int (Int, Int, [Int]))
ta_cdlmathold inOpen inHigh inLow inClose optInPenetration
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlmathold startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) (realToFrac optInPenetration) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLMORNINGDOJISTAR   Morning Doji Star
--

foreign import ccall unsafe "ta_func.h TA_CDLMORNINGDOJISTAR"
  c_ta_cdlmorningdojistar :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInPenetration (double)
-- outputs
--   outInteger (int[])

ta_cdlmorningdojistar :: [Double] -> [Double] -> [Double] -> [Double] -> Double -> IO (Either Int (Int, Int, [Int]))
ta_cdlmorningdojistar inOpen inHigh inLow inClose optInPenetration
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlmorningdojistar startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) (realToFrac optInPenetration) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLMORNINGSTAR       Morning Star
--

foreign import ccall unsafe "ta_func.h TA_CDLMORNINGSTAR"
  c_ta_cdlmorningstar :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInPenetration (double)
-- outputs
--   outInteger (int[])

ta_cdlmorningstar :: [Double] -> [Double] -> [Double] -> [Double] -> Double -> IO (Either Int (Int, Int, [Int]))
ta_cdlmorningstar inOpen inHigh inLow inClose optInPenetration
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlmorningstar startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) (realToFrac optInPenetration) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLONNECK            On-Neck Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLONNECK"
  c_ta_cdlonneck :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlonneck :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlonneck inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlonneck startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLPIERCING          Piercing Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLPIERCING"
  c_ta_cdlpiercing :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlpiercing :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlpiercing inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlpiercing startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLRICKSHAWMAN       Rickshaw Man
--

foreign import ccall unsafe "ta_func.h TA_CDLRICKSHAWMAN"
  c_ta_cdlrickshawman :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlrickshawman :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlrickshawman inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlrickshawman startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLRISEFALL3METHODS  Rising/Falling Three Methods
--

foreign import ccall unsafe "ta_func.h TA_CDLRISEFALL3METHODS"
  c_ta_cdlrisefall3methods :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlrisefall3methods :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlrisefall3methods inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlrisefall3methods startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLSEPARATINGLINES   Separating Lines
--

foreign import ccall unsafe "ta_func.h TA_CDLSEPARATINGLINES"
  c_ta_cdlseparatinglines :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlseparatinglines :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlseparatinglines inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlseparatinglines startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLSHOOTINGSTAR      Shooting Star
--

foreign import ccall unsafe "ta_func.h TA_CDLSHOOTINGSTAR"
  c_ta_cdlshootingstar :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlshootingstar :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlshootingstar inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlshootingstar startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLSHORTLINE         Short Line Candle
--

foreign import ccall unsafe "ta_func.h TA_CDLSHORTLINE"
  c_ta_cdlshortline :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlshortline :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlshortline inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlshortline startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLSPINNINGTOP       Spinning Top
--

foreign import ccall unsafe "ta_func.h TA_CDLSPINNINGTOP"
  c_ta_cdlspinningtop :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlspinningtop :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlspinningtop inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlspinningtop startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLSTALLEDPATTERN    Stalled Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLSTALLEDPATTERN"
  c_ta_cdlstalledpattern :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlstalledpattern :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlstalledpattern inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlstalledpattern startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLSTICKSANDWICH     Stick Sandwich
--

foreign import ccall unsafe "ta_func.h TA_CDLSTICKSANDWICH"
  c_ta_cdlsticksandwich :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlsticksandwich :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlsticksandwich inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlsticksandwich startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLTAKURI            Takuri (Dragonfly Doji with very long lower shadow)
--

foreign import ccall unsafe "ta_func.h TA_CDLTAKURI"
  c_ta_cdltakuri :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdltakuri :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdltakuri inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdltakuri startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLTASUKIGAP         Tasuki Gap
--

foreign import ccall unsafe "ta_func.h TA_CDLTASUKIGAP"
  c_ta_cdltasukigap :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdltasukigap :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdltasukigap inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdltasukigap startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLTHRUSTING         Thrusting Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLTHRUSTING"
  c_ta_cdlthrusting :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlthrusting :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlthrusting inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlthrusting startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLTRISTAR           Tristar Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLTRISTAR"
  c_ta_cdltristar :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdltristar :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdltristar inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdltristar startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLUNIQUE3RIVER      Unique 3 River
--

foreign import ccall unsafe "ta_func.h TA_CDLUNIQUE3RIVER"
  c_ta_cdlunique3river :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlunique3river :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlunique3river inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlunique3river startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLUPSIDEGAP2CROWS   Upside Gap Two Crows
--

foreign import ccall unsafe "ta_func.h TA_CDLUPSIDEGAP2CROWS"
  c_ta_cdlupsidegap2crows :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlupsidegap2crows :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlupsidegap2crows inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlupsidegap2crows startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CDLXSIDEGAP3METHODS  Upside/Downside Gap Three Methods
--

foreign import ccall unsafe "ta_func.h TA_CDLXSIDEGAP3METHODS"
  c_ta_cdlxsidegap3methods :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlxsidegap3methods :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Int]))
ta_cdlxsidegap3methods inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cdlxsidegap3methods startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CEIL                 Vector Ceil
--

foreign import ccall unsafe "ta_func.h TA_CEIL"
  c_ta_ceil :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_ceil :: [Double] -> IO (Either Int (Int, Int, [Double]))
ta_ceil inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_ceil startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CMO                  Chande Momentum Oscillator
--

foreign import ccall unsafe "ta_func.h TA_CMO"
  c_ta_cmo :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_cmo :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_cmo inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cmo startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- CORREL               Pearson's Correlation Coefficient (r)
--

foreign import ccall unsafe "ta_func.h TA_CORREL"
  c_ta_correl :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal0
--   inReal1
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_correl :: [Double] -> [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_correl inReal0 inReal1 optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_correl startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal0 ++ inReal1
          len = fromIntegral $ length inReal0
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- COS                  Vector Trigonometric Cos
--

foreign import ccall unsafe "ta_func.h TA_COS"
  c_ta_cos :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_cos :: [Double] -> IO (Either Int (Int, Int, [Double]))
ta_cos inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cos startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- COSH                 Vector Trigonometric Cosh
--

foreign import ccall unsafe "ta_func.h TA_COSH"
  c_ta_cosh :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_cosh :: [Double] -> IO (Either Int (Int, Int, [Double]))
ta_cosh inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_cosh startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- DEMA                 Double Exponential Moving Average
--

foreign import ccall unsafe "ta_func.h TA_DEMA"
  c_ta_dema :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_dema :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_dema inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_dema startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- DIV                  Vector Arithmetic Div
--

foreign import ccall unsafe "ta_func.h TA_DIV"
  c_ta_div :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal0
--   inReal1
-- arguments
-- outputs
--   outReal (double[])

ta_div :: [Double] -> [Double] -> IO (Either Int (Int, Int, [Double]))
ta_div inReal0 inReal1
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_div startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal0 ++ inReal1
          len = fromIntegral $ length inReal0
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- DX                   Directional Movement Index
--

foreign import ccall unsafe "ta_func.h TA_DX"
  c_ta_dx :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_dx :: [Double] -> [Double] -> [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_dx inHigh inLow inClose optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_dx startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- EMA                  Exponential Moving Average
--

foreign import ccall unsafe "ta_func.h TA_EMA"
  c_ta_ema :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_ema :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_ema inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_ema startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- EXP                  Vector Arithmetic Exp
--

foreign import ccall unsafe "ta_func.h TA_EXP"
  c_ta_exp :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_exp :: [Double] -> IO (Either Int (Int, Int, [Double]))
ta_exp inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_exp startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- FLOOR                Vector Floor
--

foreign import ccall unsafe "ta_func.h TA_FLOOR"
  c_ta_floor :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_floor :: [Double] -> IO (Either Int (Int, Int, [Double]))
ta_floor inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_floor startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- HT_DCPERIOD          Hilbert Transform - Dominant Cycle Period
--

foreign import ccall unsafe "ta_func.h TA_HT_DCPERIOD"
  c_ta_ht_dcperiod :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_ht_dcperiod :: [Double] -> IO (Either Int (Int, Int, [Double]))
ta_ht_dcperiod inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_ht_dcperiod startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- HT_DCPHASE           Hilbert Transform - Dominant Cycle Phase
--

foreign import ccall unsafe "ta_func.h TA_HT_DCPHASE"
  c_ta_ht_dcphase :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_ht_dcphase :: [Double] -> IO (Either Int (Int, Int, [Double]))
ta_ht_dcphase inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_ht_dcphase startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- HT_PHASOR            Hilbert Transform - Phasor Components
--

foreign import ccall unsafe "ta_func.h TA_HT_PHASOR"
  c_ta_ht_phasor :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outInPhase (double[])
--   outQuadrature (double[])

ta_ht_phasor :: [Double] -> IO (Either Int (Int, Int, [Double], [Double]))
ta_ht_phasor inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_ht_phasor startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0,
                                 chunks !! 1
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 2

--
-- HT_SINE              Hilbert Transform - SineWave
--

foreign import ccall unsafe "ta_func.h TA_HT_SINE"
  c_ta_ht_sine :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outSine (double[])
--   outLeadSine (double[])

ta_ht_sine :: [Double] -> IO (Either Int (Int, Int, [Double], [Double]))
ta_ht_sine inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_ht_sine startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0,
                                 chunks !! 1
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 2

--
-- HT_TRENDLINE         Hilbert Transform - Instantaneous Trendline
--

foreign import ccall unsafe "ta_func.h TA_HT_TRENDLINE"
  c_ta_ht_trendline :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_ht_trendline :: [Double] -> IO (Either Int (Int, Int, [Double]))
ta_ht_trendline inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_ht_trendline startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- HT_TRENDMODE         Hilbert Transform - Trend vs Cycle Mode
--

foreign import ccall unsafe "ta_func.h TA_HT_TRENDMODE"
  c_ta_ht_trendmode :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outInteger (int[])

ta_ht_trendmode :: [Double] -> IO (Either Int (Int, Int, [Int]))
ta_ht_trendmode inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_ht_trendmode startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- KAMA                 Kaufman Adaptive Moving Average
--

foreign import ccall unsafe "ta_func.h TA_KAMA"
  c_ta_kama :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_kama :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_kama inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_kama startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- LINEARREG            Linear Regression
--

foreign import ccall unsafe "ta_func.h TA_LINEARREG"
  c_ta_linearreg :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_linearreg :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_linearreg inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_linearreg startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- LINEARREG_ANGLE      Linear Regression Angle
--

foreign import ccall unsafe "ta_func.h TA_LINEARREG_ANGLE"
  c_ta_linearreg_angle :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_linearreg_angle :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_linearreg_angle inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_linearreg_angle startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- LINEARREG_INTERCEPT  Linear Regression Intercept
--

foreign import ccall unsafe "ta_func.h TA_LINEARREG_INTERCEPT"
  c_ta_linearreg_intercept :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_linearreg_intercept :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_linearreg_intercept inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_linearreg_intercept startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- LINEARREG_SLOPE      Linear Regression Slope
--

foreign import ccall unsafe "ta_func.h TA_LINEARREG_SLOPE"
  c_ta_linearreg_slope :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_linearreg_slope :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_linearreg_slope inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_linearreg_slope startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- LN                   Vector Log Natural
--

foreign import ccall unsafe "ta_func.h TA_LN"
  c_ta_ln :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_ln :: [Double] -> IO (Either Int (Int, Int, [Double]))
ta_ln inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_ln startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- LOG10                Vector Log10
--

foreign import ccall unsafe "ta_func.h TA_LOG10"
  c_ta_log10 :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_log10 :: [Double] -> IO (Either Int (Int, Int, [Double]))
ta_log10 inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_log10 startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- MA                   Moving average
--

foreign import ccall unsafe "ta_func.h TA_MA"
  c_ta_ma :: CInt -> CInt -> Ptr CDouble -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
--   optInMAType (int)
-- outputs
--   outReal (double[])

ta_ma :: [Double] -> Int -> Int -> IO (Either Int (Int, Int, [Double]))
ta_ma inReal optInTimePeriod optInMAType
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_ma startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) (fromIntegral optInMAType) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- MACD                 Moving Average Convergence/Divergence
--

foreign import ccall unsafe "ta_func.h TA_MACD"
  c_ta_macd :: CInt -> CInt -> Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInFastPeriod (int)
--   optInSlowPeriod (int)
--   optInSignalPeriod (int)
-- outputs
--   outMACD (double[])
--   outMACDSignal (double[])
--   outMACDHist (double[])

ta_macd :: [Double] -> Int -> Int -> Int -> IO (Either Int (Int, Int, [Double], [Double], [Double]))
ta_macd inReal optInFastPeriod optInSlowPeriod optInSignalPeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_macd startIdx endIdx (getInArrPtr 0) (fromIntegral optInFastPeriod) (fromIntegral optInSlowPeriod) (fromIntegral optInSignalPeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1) (getOutArrPtr 2)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0,
                                 chunks !! 1,
                                 chunks !! 2
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 3

--
-- MACDEXT              MACD with controllable MA type
--

foreign import ccall unsafe "ta_func.h TA_MACDEXT"
  c_ta_macdext :: CInt -> CInt -> Ptr CDouble -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInFastPeriod (int)
--   optInFastMAType (int)
--   optInSlowPeriod (int)
--   optInSlowMAType (int)
--   optInSignalPeriod (int)
--   optInSignalMAType (int)
-- outputs
--   outMACD (double[])
--   outMACDSignal (double[])
--   outMACDHist (double[])

ta_macdext :: [Double] -> Int -> Int -> Int -> Int -> Int -> Int -> IO (Either Int (Int, Int, [Double], [Double], [Double]))
ta_macdext inReal optInFastPeriod optInFastMAType optInSlowPeriod optInSlowMAType optInSignalPeriod optInSignalMAType
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_macdext startIdx endIdx (getInArrPtr 0) (fromIntegral optInFastPeriod) (fromIntegral optInFastMAType) (fromIntegral optInSlowPeriod) (fromIntegral optInSlowMAType) (fromIntegral optInSignalPeriod) (fromIntegral optInSignalMAType) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1) (getOutArrPtr 2)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0,
                                 chunks !! 1,
                                 chunks !! 2
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 3

--
-- MACDFIX              Moving Average Convergence/Divergence Fix 12/26
--

foreign import ccall unsafe "ta_func.h TA_MACDFIX"
  c_ta_macdfix :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInSignalPeriod (int)
-- outputs
--   outMACD (double[])
--   outMACDSignal (double[])
--   outMACDHist (double[])

ta_macdfix :: [Double] -> Int -> IO (Either Int (Int, Int, [Double], [Double], [Double]))
ta_macdfix inReal optInSignalPeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_macdfix startIdx endIdx (getInArrPtr 0) (fromIntegral optInSignalPeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1) (getOutArrPtr 2)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0,
                                 chunks !! 1,
                                 chunks !! 2
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 3

--
-- MAMA                 MESA Adaptive Moving Average
--

foreign import ccall unsafe "ta_func.h TA_MAMA"
  c_ta_mama :: CInt -> CInt -> Ptr CDouble -> CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInFastLimit (double)
--   optInSlowLimit (double)
-- outputs
--   outMAMA (double[])
--   outFAMA (double[])

ta_mama :: [Double] -> Double -> Double -> IO (Either Int (Int, Int, [Double], [Double]))
ta_mama inReal optInFastLimit optInSlowLimit
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_mama startIdx endIdx (getInArrPtr 0) (realToFrac optInFastLimit) (realToFrac optInSlowLimit) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0,
                                 chunks !! 1
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 2

--
-- MAVP                 Moving average with variable period
--

foreign import ccall unsafe "ta_func.h TA_MAVP"
  c_ta_mavp :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
--   inPeriods
-- arguments
--   optInMinPeriod (int)
--   optInMaxPeriod (int)
--   optInMAType (int)
-- outputs
--   outReal (double[])

ta_mavp :: [Double] -> [Double] -> Int -> Int -> Int -> IO (Either Int (Int, Int, [Double]))
ta_mavp inReal inPeriods optInMinPeriod optInMaxPeriod optInMAType
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_mavp startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (fromIntegral optInMinPeriod) (fromIntegral optInMaxPeriod) (fromIntegral optInMAType) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal ++ inPeriods
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- MAX                  Highest value over a specified period
--

foreign import ccall unsafe "ta_func.h TA_MAX"
  c_ta_max :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_max :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_max inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_max startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- MAXINDEX             Index of highest value over a specified period
--

foreign import ccall unsafe "ta_func.h TA_MAXINDEX"
  c_ta_maxindex :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outInteger (int[])

ta_maxindex :: [Double] -> Int -> IO (Either Int (Int, Int, [Int]))
ta_maxindex inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_maxindex startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- MEDPRICE             Median Price
--

foreign import ccall unsafe "ta_func.h TA_MEDPRICE"
  c_ta_medprice :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
-- arguments
-- outputs
--   outReal (double[])

ta_medprice :: [Double] -> [Double] -> IO (Either Int (Int, Int, [Double]))
ta_medprice inHigh inLow
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_medprice startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- MFI                  Money Flow Index
--

foreign import ccall unsafe "ta_func.h TA_MFI"
  c_ta_mfi :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
--   inVolume
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_mfi :: [Double] -> [Double] -> [Double] -> [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_mfi inHigh inLow inClose inVolume optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_mfi startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose ++ inVolume
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- MIDPOINT             MidPoint over period
--

foreign import ccall unsafe "ta_func.h TA_MIDPOINT"
  c_ta_midpoint :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_midpoint :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_midpoint inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_midpoint startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- MIDPRICE             Midpoint Price over period
--

foreign import ccall unsafe "ta_func.h TA_MIDPRICE"
  c_ta_midprice :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_midprice :: [Double] -> [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_midprice inHigh inLow optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_midprice startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- MIN                  Lowest value over a specified period
--

foreign import ccall unsafe "ta_func.h TA_MIN"
  c_ta_min :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_min :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_min inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_min startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- MININDEX             Index of lowest value over a specified period
--

foreign import ccall unsafe "ta_func.h TA_MININDEX"
  c_ta_minindex :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outInteger (int[])

ta_minindex :: [Double] -> Int -> IO (Either Int (Int, Int, [Int]))
ta_minindex inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_minindex startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- MINMAX               Lowest and highest values over a specified period
--

foreign import ccall unsafe "ta_func.h TA_MINMAX"
  c_ta_minmax :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outMin (double[])
--   outMax (double[])

ta_minmax :: [Double] -> Int -> IO (Either Int (Int, Int, [Double], [Double]))
ta_minmax inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_minmax startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0,
                                 chunks !! 1
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 2

--
-- MINMAXINDEX          Indexes of lowest and highest values over a specified period
--

foreign import ccall unsafe "ta_func.h TA_MINMAXINDEX"
  c_ta_minmaxindex :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outMinIdx (int[])
--   outMaxIdx (int[])

ta_minmaxindex :: [Double] -> Int -> IO (Either Int (Int, Int, [Int], [Int]))
ta_minmaxindex inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_minmaxindex startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0,
                                 chunks !! 1
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 2

--
-- MINUS_DI             Minus Directional Indicator
--

foreign import ccall unsafe "ta_func.h TA_MINUS_DI"
  c_ta_minus_di :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_minus_di :: [Double] -> [Double] -> [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_minus_di inHigh inLow inClose optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_minus_di startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- MINUS_DM             Minus Directional Movement
--

foreign import ccall unsafe "ta_func.h TA_MINUS_DM"
  c_ta_minus_dm :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_minus_dm :: [Double] -> [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_minus_dm inHigh inLow optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_minus_dm startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- MOM                  Momentum
--

foreign import ccall unsafe "ta_func.h TA_MOM"
  c_ta_mom :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_mom :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_mom inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_mom startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- MULT                 Vector Arithmetic Mult
--

foreign import ccall unsafe "ta_func.h TA_MULT"
  c_ta_mult :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal0
--   inReal1
-- arguments
-- outputs
--   outReal (double[])

ta_mult :: [Double] -> [Double] -> IO (Either Int (Int, Int, [Double]))
ta_mult inReal0 inReal1
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_mult startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal0 ++ inReal1
          len = fromIntegral $ length inReal0
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- NATR                 Normalized Average True Range
--

foreign import ccall unsafe "ta_func.h TA_NATR"
  c_ta_natr :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_natr :: [Double] -> [Double] -> [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_natr inHigh inLow inClose optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_natr startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- OBV                  On Balance Volume
--

foreign import ccall unsafe "ta_func.h TA_OBV"
  c_ta_obv :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
--   inVolume1
-- arguments
-- outputs
--   outReal (double[])

ta_obv :: [Double] -> [Double] -> IO (Either Int (Int, Int, [Double]))
ta_obv inReal inVolume1
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_obv startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal ++ inVolume1
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- PLUS_DI              Plus Directional Indicator
--

foreign import ccall unsafe "ta_func.h TA_PLUS_DI"
  c_ta_plus_di :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_plus_di :: [Double] -> [Double] -> [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_plus_di inHigh inLow inClose optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_plus_di startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- PLUS_DM              Plus Directional Movement
--

foreign import ccall unsafe "ta_func.h TA_PLUS_DM"
  c_ta_plus_dm :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_plus_dm :: [Double] -> [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_plus_dm inHigh inLow optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_plus_dm startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- PPO                  Percentage Price Oscillator
--

foreign import ccall unsafe "ta_func.h TA_PPO"
  c_ta_ppo :: CInt -> CInt -> Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInFastPeriod (int)
--   optInSlowPeriod (int)
--   optInMAType (int)
-- outputs
--   outReal (double[])

ta_ppo :: [Double] -> Int -> Int -> Int -> IO (Either Int (Int, Int, [Double]))
ta_ppo inReal optInFastPeriod optInSlowPeriod optInMAType
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_ppo startIdx endIdx (getInArrPtr 0) (fromIntegral optInFastPeriod) (fromIntegral optInSlowPeriod) (fromIntegral optInMAType) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- ROC                  Rate of change : ((price/prevPrice)-1)*100
--

foreign import ccall unsafe "ta_func.h TA_ROC"
  c_ta_roc :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_roc :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_roc inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_roc startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- ROCP                 Rate of change Percentage: (price-prevPrice)/prevPrice
--

foreign import ccall unsafe "ta_func.h TA_ROCP"
  c_ta_rocp :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_rocp :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_rocp inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_rocp startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- ROCR                 Rate of change ratio: (price/prevPrice)
--

foreign import ccall unsafe "ta_func.h TA_ROCR"
  c_ta_rocr :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_rocr :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_rocr inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_rocr startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- ROCR100              Rate of change ratio 100 scale: (price/prevPrice)*100
--

foreign import ccall unsafe "ta_func.h TA_ROCR100"
  c_ta_rocr100 :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_rocr100 :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_rocr100 inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_rocr100 startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- RSI                  Relative Strength Index
--

foreign import ccall unsafe "ta_func.h TA_RSI"
  c_ta_rsi :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_rsi :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_rsi inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_rsi startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- SAR                  Parabolic SAR
--

foreign import ccall unsafe "ta_func.h TA_SAR"
  c_ta_sar :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
-- arguments
--   optInAcceleration (double)
--   optInMaximum (double)
-- outputs
--   outReal (double[])

ta_sar :: [Double] -> [Double] -> Double -> Double -> IO (Either Int (Int, Int, [Double]))
ta_sar inHigh inLow optInAcceleration optInMaximum
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_sar startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (realToFrac optInAcceleration) (realToFrac optInMaximum) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- SAREXT               Parabolic SAR - Extended
--

foreign import ccall unsafe "ta_func.h TA_SAREXT"
  c_ta_sarext :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
-- arguments
--   optInStartValue (double)
--   optInOffsetOnReverse (double)
--   optInAccelerationInitLong (double)
--   optInAccelerationLong (double)
--   optInAccelerationMaxLong (double)
--   optInAccelerationInitShort (double)
--   optInAccelerationShort (double)
--   optInAccelerationMaxShort (double)
-- outputs
--   outReal (double[])

ta_sarext :: [Double] -> [Double] -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO (Either Int (Int, Int, [Double]))
ta_sarext inHigh inLow optInStartValue optInOffsetOnReverse optInAccelerationInitLong optInAccelerationLong optInAccelerationMaxLong optInAccelerationInitShort optInAccelerationShort optInAccelerationMaxShort
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_sarext startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (realToFrac optInStartValue) (realToFrac optInOffsetOnReverse) (realToFrac optInAccelerationInitLong) (realToFrac optInAccelerationLong) (realToFrac optInAccelerationMaxLong) (realToFrac optInAccelerationInitShort) (realToFrac optInAccelerationShort) (realToFrac optInAccelerationMaxShort) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- SIN                  Vector Trigonometric Sin
--

foreign import ccall unsafe "ta_func.h TA_SIN"
  c_ta_sin :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_sin :: [Double] -> IO (Either Int (Int, Int, [Double]))
ta_sin inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_sin startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- SINH                 Vector Trigonometric Sinh
--

foreign import ccall unsafe "ta_func.h TA_SINH"
  c_ta_sinh :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_sinh :: [Double] -> IO (Either Int (Int, Int, [Double]))
ta_sinh inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_sinh startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- SMA                  Simple Moving Average
--

foreign import ccall unsafe "ta_func.h TA_SMA"
  c_ta_sma :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_sma :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_sma inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_sma startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- SQRT                 Vector Square Root
--

foreign import ccall unsafe "ta_func.h TA_SQRT"
  c_ta_sqrt :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_sqrt :: [Double] -> IO (Either Int (Int, Int, [Double]))
ta_sqrt inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_sqrt startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- STDDEV               Standard Deviation
--

foreign import ccall unsafe "ta_func.h TA_STDDEV"
  c_ta_stddev :: CInt -> CInt -> Ptr CDouble -> CInt -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
--   optInNbDev (double)
-- outputs
--   outReal (double[])

ta_stddev :: [Double] -> Int -> Double -> IO (Either Int (Int, Int, [Double]))
ta_stddev inReal optInTimePeriod optInNbDev
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_stddev startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) (realToFrac optInNbDev) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- STOCH                Stochastic
--

foreign import ccall unsafe "ta_func.h TA_STOCH"
  c_ta_stoch :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInFastK_Period (int)
--   optInSlowK_Period (int)
--   optInSlowK_MAType (int)
--   optInSlowD_Period (int)
--   optInSlowD_MAType (int)
-- outputs
--   outSlowK (double[])
--   outSlowD (double[])

ta_stoch :: [Double] -> [Double] -> [Double] -> Int -> Int -> Int -> Int -> Int -> IO (Either Int (Int, Int, [Double], [Double]))
ta_stoch inHigh inLow inClose optInFastK_Period optInSlowK_Period optInSlowK_MAType optInSlowD_Period optInSlowD_MAType
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_stoch startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInFastK_Period) (fromIntegral optInSlowK_Period) (fromIntegral optInSlowK_MAType) (fromIntegral optInSlowD_Period) (fromIntegral optInSlowD_MAType) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0,
                                 chunks !! 1
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 2

--
-- STOCHF               Stochastic Fast
--

foreign import ccall unsafe "ta_func.h TA_STOCHF"
  c_ta_stochf :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInFastK_Period (int)
--   optInFastD_Period (int)
--   optInFastD_MAType (int)
-- outputs
--   outFastK (double[])
--   outFastD (double[])

ta_stochf :: [Double] -> [Double] -> [Double] -> Int -> Int -> Int -> IO (Either Int (Int, Int, [Double], [Double]))
ta_stochf inHigh inLow inClose optInFastK_Period optInFastD_Period optInFastD_MAType
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_stochf startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInFastK_Period) (fromIntegral optInFastD_Period) (fromIntegral optInFastD_MAType) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0,
                                 chunks !! 1
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 2

--
-- STOCHRSI             Stochastic Relative Strength Index
--

foreign import ccall unsafe "ta_func.h TA_STOCHRSI"
  c_ta_stochrsi :: CInt -> CInt -> Ptr CDouble -> CInt -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
--   optInFastK_Period (int)
--   optInFastD_Period (int)
--   optInFastD_MAType (int)
-- outputs
--   outFastK (double[])
--   outFastD (double[])

ta_stochrsi :: [Double] -> Int -> Int -> Int -> Int -> IO (Either Int (Int, Int, [Double], [Double]))
ta_stochrsi inReal optInTimePeriod optInFastK_Period optInFastD_Period optInFastD_MAType
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_stochrsi startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) (fromIntegral optInFastK_Period) (fromIntegral optInFastD_Period) (fromIntegral optInFastD_MAType) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0,
                                 chunks !! 1
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 2

--
-- SUB                  Vector Arithmetic Substraction
--

foreign import ccall unsafe "ta_func.h TA_SUB"
  c_ta_sub :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal0
--   inReal1
-- arguments
-- outputs
--   outReal (double[])

ta_sub :: [Double] -> [Double] -> IO (Either Int (Int, Int, [Double]))
ta_sub inReal0 inReal1
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_sub startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal0 ++ inReal1
          len = fromIntegral $ length inReal0
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- SUM                  Summation
--

foreign import ccall unsafe "ta_func.h TA_SUM"
  c_ta_sum :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_sum :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_sum inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_sum startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- T3                   Triple Exponential Moving Average (T3)
--

foreign import ccall unsafe "ta_func.h TA_T3"
  c_ta_t3 :: CInt -> CInt -> Ptr CDouble -> CInt -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
--   optInVFactor (double)
-- outputs
--   outReal (double[])

ta_t3 :: [Double] -> Int -> Double -> IO (Either Int (Int, Int, [Double]))
ta_t3 inReal optInTimePeriod optInVFactor
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_t3 startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) (realToFrac optInVFactor) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- TAN                  Vector Trigonometric Tan
--

foreign import ccall unsafe "ta_func.h TA_TAN"
  c_ta_tan :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_tan :: [Double] -> IO (Either Int (Int, Int, [Double]))
ta_tan inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_tan startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- TANH                 Vector Trigonometric Tanh
--

foreign import ccall unsafe "ta_func.h TA_TANH"
  c_ta_tanh :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_tanh :: [Double] -> IO (Either Int (Int, Int, [Double]))
ta_tanh inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_tanh startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- TEMA                 Triple Exponential Moving Average
--

foreign import ccall unsafe "ta_func.h TA_TEMA"
  c_ta_tema :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_tema :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_tema inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_tema startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- TRANGE               True Range
--

foreign import ccall unsafe "ta_func.h TA_TRANGE"
  c_ta_trange :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outReal (double[])

ta_trange :: [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Double]))
ta_trange inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_trange startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- TRIMA                Triangular Moving Average
--

foreign import ccall unsafe "ta_func.h TA_TRIMA"
  c_ta_trima :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_trima :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_trima inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_trima startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- TRIX                 1-day Rate-Of-Change (ROC) of a Triple Smooth EMA
--

foreign import ccall unsafe "ta_func.h TA_TRIX"
  c_ta_trix :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_trix :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_trix inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_trix startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- TSF                  Time Series Forecast
--

foreign import ccall unsafe "ta_func.h TA_TSF"
  c_ta_tsf :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_tsf :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_tsf inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_tsf startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- TYPPRICE             Typical Price
--

foreign import ccall unsafe "ta_func.h TA_TYPPRICE"
  c_ta_typprice :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outReal (double[])

ta_typprice :: [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Double]))
ta_typprice inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_typprice startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- ULTOSC               Ultimate Oscillator
--

foreign import ccall unsafe "ta_func.h TA_ULTOSC"
  c_ta_ultosc :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInTimePeriod1 (int)
--   optInTimePeriod2 (int)
--   optInTimePeriod3 (int)
-- outputs
--   outReal (double[])

ta_ultosc :: [Double] -> [Double] -> [Double] -> Int -> Int -> Int -> IO (Either Int (Int, Int, [Double]))
ta_ultosc inHigh inLow inClose optInTimePeriod1 optInTimePeriod2 optInTimePeriod3
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_ultosc startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInTimePeriod1) (fromIntegral optInTimePeriod2) (fromIntegral optInTimePeriod3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- VAR                  Variance
--

foreign import ccall unsafe "ta_func.h TA_VAR"
  c_ta_var :: CInt -> CInt -> Ptr CDouble -> CInt -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
--   optInNbDev (double)
-- outputs
--   outReal (double[])

ta_var :: [Double] -> Int -> Double -> IO (Either Int (Int, Int, [Double]))
ta_var inReal optInTimePeriod optInNbDev
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_var startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) (realToFrac optInNbDev) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- WCLPRICE             Weighted Close Price
--

foreign import ccall unsafe "ta_func.h TA_WCLPRICE"
  c_ta_wclprice :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outReal (double[])

ta_wclprice :: [Double] -> [Double] -> [Double] -> IO (Either Int (Int, Int, [Double]))
ta_wclprice inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_wclprice startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- WILLR                Williams' %R
--

foreign import ccall unsafe "ta_func.h TA_WILLR"
  c_ta_willr :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_willr :: [Double] -> [Double] -> [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_willr inHigh inLow inClose optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_willr startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

--
-- WMA                  Weighted Moving Average
--

foreign import ccall unsafe "ta_func.h TA_WMA"
  c_ta_wma :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_wma :: [Double] -> Int -> IO (Either Int (Int, Int, [Double]))
ta_wma inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOut ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOut
      in do
        rc <- c_ta_wma startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               _out <- peekArray (len * outputs) cOut
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               let chunks = (chunksOf len _out)
               return $ Right $ (fromIntegral outBegIdx,
                                 fromIntegral outNbElement,
                                 chunks !! 0
                                )
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1


-- *******************************
-- *** End Auto-Generated Code ***
-- *******************************

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

        putStrLn "Relative Strength Index"
        result <- ta_rsi inReal 9
        print result
        terpri
        
        
        
        
        
        
        
        