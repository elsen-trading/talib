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

-- TA-Lib : Technical Analysis Library

foreign import ccall unsafe "ta_common.h TA_Initialize"
  c_ta_init :: IO ()

-- // Start

-- ACOS                 Vector Trigonometric ACos

foreign import ccall unsafe "ta_func.h TA_ACOS"
  c_ta_acos :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_acos :: [Double] -> IO (Either Int TaOutput)
ta_acos inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_acos startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- AD                   Chaikin A/D Line

foreign import ccall unsafe "ta_func.h TA_AD"
  c_ta_ad :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_ad :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int TaOutput)
ta_ad inHigh inLow inClose inVolume
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_ad startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose ++ inVolume
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- ADD                  Vector Arithmetic Add

foreign import ccall unsafe "ta_func.h TA_ADD"
  c_ta_add :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_add :: [Double] -> [Double] -> IO (Either Int TaOutput)
ta_add inReal0 inReal1
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_add startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal0 ++ inReal1
          len = fromIntegral $ length inReal0
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- ADOSC                Chaikin A/D Oscillator

foreign import ccall unsafe "ta_func.h TA_ADOSC"
  c_ta_adosc :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_adosc :: [Double] -> [Double] -> [Double] -> [Double] -> Int -> Int -> IO (Either Int TaOutput)
ta_adosc inHigh inLow inClose inVolume optInFastPeriod optInSlowPeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_adosc startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) (fromIntegral optInFastPeriod) (fromIntegral optInSlowPeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose ++ inVolume
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- ADX                  Average Directional Movement Index

foreign import ccall unsafe "ta_func.h TA_ADX"
  c_ta_adx :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_adx :: [Double] -> [Double] -> [Double] -> Int -> IO (Either Int TaOutput)
ta_adx inHigh inLow inClose optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_adx startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- ADXR                 Average Directional Movement Index Rating

foreign import ccall unsafe "ta_func.h TA_ADXR"
  c_ta_adxr :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_adxr :: [Double] -> [Double] -> [Double] -> Int -> IO (Either Int TaOutput)
ta_adxr inHigh inLow inClose optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_adxr startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- APO                  Absolute Price Oscillator

foreign import ccall unsafe "ta_func.h TA_APO"
  c_ta_apo :: CInt -> CInt -> Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_apo :: [Double] -> Int -> Int -> Int -> IO (Either Int TaOutput)
ta_apo inReal optInFastPeriod optInSlowPeriod optInMAType
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_apo startIdx endIdx (getInArrPtr 0) (fromIntegral optInFastPeriod) (fromIntegral optInSlowPeriod) (fromIntegral optInMAType) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- AROON                Aroon

foreign import ccall unsafe "ta_func.h TA_AROON"
  c_ta_aroon :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_aroon :: [Double] -> [Double] -> Int -> IO (Either Int TaOutput)
ta_aroon inHigh inLow optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_aroon startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 2

-- AROONOSC             Aroon Oscillator

foreign import ccall unsafe "ta_func.h TA_AROONOSC"
  c_ta_aroonosc :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_aroonosc :: [Double] -> [Double] -> Int -> IO (Either Int TaOutput)
ta_aroonosc inHigh inLow optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_aroonosc startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- ASIN                 Vector Trigonometric ASin

foreign import ccall unsafe "ta_func.h TA_ASIN"
  c_ta_asin :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_asin :: [Double] -> IO (Either Int TaOutput)
ta_asin inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_asin startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- ATAN                 Vector Trigonometric ATan

foreign import ccall unsafe "ta_func.h TA_ATAN"
  c_ta_atan :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_atan :: [Double] -> IO (Either Int TaOutput)
ta_atan inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_atan startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- ATR                  Average True Range

foreign import ccall unsafe "ta_func.h TA_ATR"
  c_ta_atr :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_atr :: [Double] -> [Double] -> [Double] -> Int -> IO (Either Int TaOutput)
ta_atr inHigh inLow inClose optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_atr startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- AVGPRICE             Average Price

foreign import ccall unsafe "ta_func.h TA_AVGPRICE"
  c_ta_avgprice :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_avgprice :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int TaOutput)
ta_avgprice inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_avgprice startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- BBANDS               Bollinger Bands

foreign import ccall unsafe "ta_func.h TA_BBANDS"
  c_ta_bbands :: CInt -> CInt -> Ptr CDouble -> CInt -> CDouble -> CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_bbands :: [Double] -> Int -> Double -> Double -> Int -> IO (Either Int TaOutput)
ta_bbands inReal optInTimePeriod optInNbDevUp optInNbDevDn optInMAType
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_bbands startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) (realToFrac optInNbDevUp) (realToFrac optInNbDevDn) (fromIntegral optInMAType) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1) (getOutArrPtr 2)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 3

-- BETA                 Beta

foreign import ccall unsafe "ta_func.h TA_BETA"
  c_ta_beta :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_beta :: [Double] -> [Double] -> Int -> IO (Either Int TaOutput)
ta_beta inReal0 inReal1 optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_beta startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal0 ++ inReal1
          len = fromIntegral $ length inReal0
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- BOP                  Balance Of Power

foreign import ccall unsafe "ta_func.h TA_BOP"
  c_ta_bop :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_bop :: [Double] -> [Double] -> [Double] -> [Double] -> IO (Either Int TaOutput)
ta_bop inOpen inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_bop startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inOpen ++ inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inOpen
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- CCI                  Commodity Channel Index

foreign import ccall unsafe "ta_func.h TA_CCI"
  c_ta_cci :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_cci :: [Double] -> [Double] -> [Double] -> Int -> IO (Either Int TaOutput)
ta_cci inHigh inLow inClose optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_cci startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- CDL2CROWS            Two Crows

-- Currently Unsupported

-- CDL3BLACKCROWS       Three Black Crows

-- Currently Unsupported

-- CDL3INSIDE           Three Inside Up/Down

-- Currently Unsupported

-- CDL3LINESTRIKE       Three-Line Strike 

-- Currently Unsupported

-- CDL3OUTSIDE          Three Outside Up/Down

-- Currently Unsupported

-- CDL3STARSINSOUTH     Three Stars In The South

-- Currently Unsupported

-- CDL3WHITESOLDIERS    Three Advancing White Soldiers

-- Currently Unsupported

-- CDLABANDONEDBABY     Abandoned Baby

-- Currently Unsupported

-- CDLADVANCEBLOCK      Advance Block

-- Currently Unsupported

-- CDLBELTHOLD          Belt-hold

-- Currently Unsupported

-- CDLBREAKAWAY         Breakaway

-- Currently Unsupported

-- CDLCLOSINGMARUBOZU   Closing Marubozu

-- Currently Unsupported

-- CDLCONCEALBABYSWALL  Concealing Baby Swallow

-- Currently Unsupported

-- CDLCOUNTERATTACK     Counterattack

-- Currently Unsupported

-- CDLDARKCLOUDCOVER    Dark Cloud Cover

-- Currently Unsupported

-- CDLDOJI              Doji

-- Currently Unsupported

-- CDLDOJISTAR          Doji Star

-- Currently Unsupported

-- CDLDRAGONFLYDOJI     Dragonfly Doji

-- Currently Unsupported

-- CDLENGULFING         Engulfing Pattern

-- Currently Unsupported

-- CDLEVENINGDOJISTAR   Evening Doji Star

-- Currently Unsupported

-- CDLEVENINGSTAR       Evening Star

-- Currently Unsupported

-- CDLGAPSIDESIDEWHITE  Up/Down-gap side-by-side white lines

-- Currently Unsupported

-- CDLGRAVESTONEDOJI    Gravestone Doji

-- Currently Unsupported

-- CDLHAMMER            Hammer

-- Currently Unsupported

-- CDLHANGINGMAN        Hanging Man

-- Currently Unsupported

-- CDLHARAMI            Harami Pattern

-- Currently Unsupported

-- CDLHARAMICROSS       Harami Cross Pattern

-- Currently Unsupported

-- CDLHIGHWAVE          High-Wave Candle

-- Currently Unsupported

-- CDLHIKKAKE           Hikkake Pattern

-- Currently Unsupported

-- CDLHIKKAKEMOD        Modified Hikkake Pattern

-- Currently Unsupported

-- CDLHOMINGPIGEON      Homing Pigeon

-- Currently Unsupported

-- CDLIDENTICAL3CROWS   Identical Three Crows

-- Currently Unsupported

-- CDLINNECK            In-Neck Pattern

-- Currently Unsupported

-- CDLINVERTEDHAMMER    Inverted Hammer

-- Currently Unsupported

-- CDLKICKING           Kicking

-- Currently Unsupported

-- CDLKICKINGBYLENGTH   Kicking - bull/bear determined by the longer marubozu

-- Currently Unsupported

-- CDLLADDERBOTTOM      Ladder Bottom

-- Currently Unsupported

-- CDLLONGLEGGEDDOJI    Long Legged Doji

-- Currently Unsupported

-- CDLLONGLINE          Long Line Candle

-- Currently Unsupported

-- CDLMARUBOZU          Marubozu

-- Currently Unsupported

-- CDLMATCHINGLOW       Matching Low

-- Currently Unsupported

-- CDLMATHOLD           Mat Hold

-- Currently Unsupported

-- CDLMORNINGDOJISTAR   Morning Doji Star

-- Currently Unsupported

-- CDLMORNINGSTAR       Morning Star

-- Currently Unsupported

-- CDLONNECK            On-Neck Pattern

-- Currently Unsupported

-- CDLPIERCING          Piercing Pattern

-- Currently Unsupported

-- CDLRICKSHAWMAN       Rickshaw Man

-- Currently Unsupported

-- CDLRISEFALL3METHODS  Rising/Falling Three Methods

-- Currently Unsupported

-- CDLSEPARATINGLINES   Separating Lines

-- Currently Unsupported

-- CDLSHOOTINGSTAR      Shooting Star

-- Currently Unsupported

-- CDLSHORTLINE         Short Line Candle

-- Currently Unsupported

-- CDLSPINNINGTOP       Spinning Top

-- Currently Unsupported

-- CDLSTALLEDPATTERN    Stalled Pattern

-- Currently Unsupported

-- CDLSTICKSANDWICH     Stick Sandwich

-- Currently Unsupported

-- CDLTAKURI            Takuri (Dragonfly Doji with very long lower shadow)

-- Currently Unsupported

-- CDLTASUKIGAP         Tasuki Gap

-- Currently Unsupported

-- CDLTHRUSTING         Thrusting Pattern

-- Currently Unsupported

-- CDLTRISTAR           Tristar Pattern

-- Currently Unsupported

-- CDLUNIQUE3RIVER      Unique 3 River

-- Currently Unsupported

-- CDLUPSIDEGAP2CROWS   Upside Gap Two Crows

-- Currently Unsupported

-- CDLXSIDEGAP3METHODS  Upside/Downside Gap Three Methods

-- Currently Unsupported

-- CEIL                 Vector Ceil

foreign import ccall unsafe "ta_func.h TA_CEIL"
  c_ta_ceil :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_ceil :: [Double] -> IO (Either Int TaOutput)
ta_ceil inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_ceil startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- CMO                  Chande Momentum Oscillator

foreign import ccall unsafe "ta_func.h TA_CMO"
  c_ta_cmo :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_cmo :: [Double] -> Int -> IO (Either Int TaOutput)
ta_cmo inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_cmo startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- CORREL               Pearson's Correlation Coefficient (r)

foreign import ccall unsafe "ta_func.h TA_CORREL"
  c_ta_correl :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_correl :: [Double] -> [Double] -> Int -> IO (Either Int TaOutput)
ta_correl inReal0 inReal1 optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_correl startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal0 ++ inReal1
          len = fromIntegral $ length inReal0
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- COS                  Vector Trigonometric Cos

foreign import ccall unsafe "ta_func.h TA_COS"
  c_ta_cos :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_cos :: [Double] -> IO (Either Int TaOutput)
ta_cos inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_cos startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- COSH                 Vector Trigonometric Cosh

foreign import ccall unsafe "ta_func.h TA_COSH"
  c_ta_cosh :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_cosh :: [Double] -> IO (Either Int TaOutput)
ta_cosh inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_cosh startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- DEMA                 Double Exponential Moving Average

foreign import ccall unsafe "ta_func.h TA_DEMA"
  c_ta_dema :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_dema :: [Double] -> Int -> IO (Either Int TaOutput)
ta_dema inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_dema startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- DIV                  Vector Arithmetic Div

foreign import ccall unsafe "ta_func.h TA_DIV"
  c_ta_div :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_div :: [Double] -> [Double] -> IO (Either Int TaOutput)
ta_div inReal0 inReal1
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_div startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal0 ++ inReal1
          len = fromIntegral $ length inReal0
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- DX                   Directional Movement Index

foreign import ccall unsafe "ta_func.h TA_DX"
  c_ta_dx :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_dx :: [Double] -> [Double] -> [Double] -> Int -> IO (Either Int TaOutput)
ta_dx inHigh inLow inClose optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_dx startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- EMA                  Exponential Moving Average

foreign import ccall unsafe "ta_func.h TA_EMA"
  c_ta_ema :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_ema :: [Double] -> Int -> IO (Either Int TaOutput)
ta_ema inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_ema startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- EXP                  Vector Arithmetic Exp

foreign import ccall unsafe "ta_func.h TA_EXP"
  c_ta_exp :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_exp :: [Double] -> IO (Either Int TaOutput)
ta_exp inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_exp startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- FLOOR                Vector Floor

foreign import ccall unsafe "ta_func.h TA_FLOOR"
  c_ta_floor :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_floor :: [Double] -> IO (Either Int TaOutput)
ta_floor inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_floor startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- HT_DCPERIOD          Hilbert Transform - Dominant Cycle Period

foreign import ccall unsafe "ta_func.h TA_HT_DCPERIOD"
  c_ta_ht_dcperiod :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_ht_dcperiod :: [Double] -> IO (Either Int TaOutput)
ta_ht_dcperiod inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_ht_dcperiod startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- HT_DCPHASE           Hilbert Transform - Dominant Cycle Phase

foreign import ccall unsafe "ta_func.h TA_HT_DCPHASE"
  c_ta_ht_dcphase :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_ht_dcphase :: [Double] -> IO (Either Int TaOutput)
ta_ht_dcphase inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_ht_dcphase startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- HT_PHASOR            Hilbert Transform - Phasor Components

foreign import ccall unsafe "ta_func.h TA_HT_PHASOR"
  c_ta_ht_phasor :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_ht_phasor :: [Double] -> IO (Either Int TaOutput)
ta_ht_phasor inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_ht_phasor startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 2

-- HT_SINE              Hilbert Transform - SineWave

foreign import ccall unsafe "ta_func.h TA_HT_SINE"
  c_ta_ht_sine :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_ht_sine :: [Double] -> IO (Either Int TaOutput)
ta_ht_sine inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_ht_sine startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 2

-- HT_TRENDLINE         Hilbert Transform - Instantaneous Trendline

foreign import ccall unsafe "ta_func.h TA_HT_TRENDLINE"
  c_ta_ht_trendline :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_ht_trendline :: [Double] -> IO (Either Int TaOutput)
ta_ht_trendline inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_ht_trendline startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- HT_TRENDMODE         Hilbert Transform - Trend vs Cycle Mode

-- Currently Unsupported

-- KAMA                 Kaufman Adaptive Moving Average

foreign import ccall unsafe "ta_func.h TA_KAMA"
  c_ta_kama :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_kama :: [Double] -> Int -> IO (Either Int TaOutput)
ta_kama inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_kama startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- LINEARREG            Linear Regression

foreign import ccall unsafe "ta_func.h TA_LINEARREG"
  c_ta_linearreg :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_linearreg :: [Double] -> Int -> IO (Either Int TaOutput)
ta_linearreg inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_linearreg startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- LINEARREG_ANGLE      Linear Regression Angle

foreign import ccall unsafe "ta_func.h TA_LINEARREG_ANGLE"
  c_ta_linearreg_angle :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_linearreg_angle :: [Double] -> Int -> IO (Either Int TaOutput)
ta_linearreg_angle inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_linearreg_angle startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- LINEARREG_INTERCEPT  Linear Regression Intercept

foreign import ccall unsafe "ta_func.h TA_LINEARREG_INTERCEPT"
  c_ta_linearreg_intercept :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_linearreg_intercept :: [Double] -> Int -> IO (Either Int TaOutput)
ta_linearreg_intercept inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_linearreg_intercept startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- LINEARREG_SLOPE      Linear Regression Slope

foreign import ccall unsafe "ta_func.h TA_LINEARREG_SLOPE"
  c_ta_linearreg_slope :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_linearreg_slope :: [Double] -> Int -> IO (Either Int TaOutput)
ta_linearreg_slope inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_linearreg_slope startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- LN                   Vector Log Natural

foreign import ccall unsafe "ta_func.h TA_LN"
  c_ta_ln :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_ln :: [Double] -> IO (Either Int TaOutput)
ta_ln inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_ln startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- LOG10                Vector Log10

foreign import ccall unsafe "ta_func.h TA_LOG10"
  c_ta_log10 :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_log10 :: [Double] -> IO (Either Int TaOutput)
ta_log10 inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_log10 startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- MA                   Moving average

foreign import ccall unsafe "ta_func.h TA_MA"
  c_ta_ma :: CInt -> CInt -> Ptr CDouble -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_ma :: [Double] -> Int -> Int -> IO (Either Int TaOutput)
ta_ma inReal optInTimePeriod optInMAType
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_ma startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) (fromIntegral optInMAType) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- MACD                 Moving Average Convergence/Divergence

foreign import ccall unsafe "ta_func.h TA_MACD"
  c_ta_macd :: CInt -> CInt -> Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_macd :: [Double] -> Int -> Int -> Int -> IO (Either Int TaOutput)
ta_macd inReal optInFastPeriod optInSlowPeriod optInSignalPeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_macd startIdx endIdx (getInArrPtr 0) (fromIntegral optInFastPeriod) (fromIntegral optInSlowPeriod) (fromIntegral optInSignalPeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1) (getOutArrPtr 2)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 3

-- MACDEXT              MACD with controllable MA type

foreign import ccall unsafe "ta_func.h TA_MACDEXT"
  c_ta_macdext :: CInt -> CInt -> Ptr CDouble -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_macdext :: [Double] -> Int -> Int -> Int -> Int -> Int -> Int -> IO (Either Int TaOutput)
ta_macdext inReal optInFastPeriod optInFastMAType optInSlowPeriod optInSlowMAType optInSignalPeriod optInSignalMAType
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_macdext startIdx endIdx (getInArrPtr 0) (fromIntegral optInFastPeriod) (fromIntegral optInFastMAType) (fromIntegral optInSlowPeriod) (fromIntegral optInSlowMAType) (fromIntegral optInSignalPeriod) (fromIntegral optInSignalMAType) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1) (getOutArrPtr 2)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 3

-- MACDFIX              Moving Average Convergence/Divergence Fix 12/26

foreign import ccall unsafe "ta_func.h TA_MACDFIX"
  c_ta_macdfix :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_macdfix :: [Double] -> Int -> IO (Either Int TaOutput)
ta_macdfix inReal optInSignalPeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_macdfix startIdx endIdx (getInArrPtr 0) (fromIntegral optInSignalPeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1) (getOutArrPtr 2)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 3

-- MAMA                 MESA Adaptive Moving Average

foreign import ccall unsafe "ta_func.h TA_MAMA"
  c_ta_mama :: CInt -> CInt -> Ptr CDouble -> CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_mama :: [Double] -> Double -> Double -> IO (Either Int TaOutput)
ta_mama inReal optInFastLimit optInSlowLimit
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_mama startIdx endIdx (getInArrPtr 0) (realToFrac optInFastLimit) (realToFrac optInSlowLimit) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 2

-- MAVP                 Moving average with variable period

foreign import ccall unsafe "ta_func.h TA_MAVP"
  c_ta_mavp :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_mavp :: [Double] -> [Double] -> Int -> Int -> Int -> IO (Either Int TaOutput)
ta_mavp inReal inPeriods optInMinPeriod optInMaxPeriod optInMAType
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_mavp startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (fromIntegral optInMinPeriod) (fromIntegral optInMaxPeriod) (fromIntegral optInMAType) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal ++ inPeriods
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- MAX                  Highest value over a specified period

foreign import ccall unsafe "ta_func.h TA_MAX"
  c_ta_max :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_max :: [Double] -> Int -> IO (Either Int TaOutput)
ta_max inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_max startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- MAXINDEX             Index of highest value over a specified period

-- Currently Unsupported

-- MEDPRICE             Median Price

foreign import ccall unsafe "ta_func.h TA_MEDPRICE"
  c_ta_medprice :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_medprice :: [Double] -> [Double] -> IO (Either Int TaOutput)
ta_medprice inHigh inLow
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_medprice startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- MFI                  Money Flow Index

foreign import ccall unsafe "ta_func.h TA_MFI"
  c_ta_mfi :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_mfi :: [Double] -> [Double] -> [Double] -> [Double] -> Int -> IO (Either Int TaOutput)
ta_mfi inHigh inLow inClose inVolume optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_mfi startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (getInArrPtr 3) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose ++ inVolume
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- MIDPOINT             MidPoint over period

foreign import ccall unsafe "ta_func.h TA_MIDPOINT"
  c_ta_midpoint :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_midpoint :: [Double] -> Int -> IO (Either Int TaOutput)
ta_midpoint inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_midpoint startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- MIDPRICE             Midpoint Price over period

foreign import ccall unsafe "ta_func.h TA_MIDPRICE"
  c_ta_midprice :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_midprice :: [Double] -> [Double] -> Int -> IO (Either Int TaOutput)
ta_midprice inHigh inLow optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_midprice startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- MIN                  Lowest value over a specified period

foreign import ccall unsafe "ta_func.h TA_MIN"
  c_ta_min :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_min :: [Double] -> Int -> IO (Either Int TaOutput)
ta_min inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_min startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- MININDEX             Index of lowest value over a specified period

-- Currently Unsupported

-- MINMAX               Lowest and highest values over a specified period

foreign import ccall unsafe "ta_func.h TA_MINMAX"
  c_ta_minmax :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_minmax :: [Double] -> Int -> IO (Either Int TaOutput)
ta_minmax inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_minmax startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 2

-- MINMAXINDEX          Indexes of lowest and highest values over a specified period

-- Currently Unsupported

-- MINUS_DI             Minus Directional Indicator

foreign import ccall unsafe "ta_func.h TA_MINUS_DI"
  c_ta_minus_di :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_minus_di :: [Double] -> [Double] -> [Double] -> Int -> IO (Either Int TaOutput)
ta_minus_di inHigh inLow inClose optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_minus_di startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- MINUS_DM             Minus Directional Movement

foreign import ccall unsafe "ta_func.h TA_MINUS_DM"
  c_ta_minus_dm :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_minus_dm :: [Double] -> [Double] -> Int -> IO (Either Int TaOutput)
ta_minus_dm inHigh inLow optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_minus_dm startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- MOM                  Momentum

foreign import ccall unsafe "ta_func.h TA_MOM"
  c_ta_mom :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_mom :: [Double] -> Int -> IO (Either Int TaOutput)
ta_mom inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_mom startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- MULT                 Vector Arithmetic Mult

foreign import ccall unsafe "ta_func.h TA_MULT"
  c_ta_mult :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_mult :: [Double] -> [Double] -> IO (Either Int TaOutput)
ta_mult inReal0 inReal1
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_mult startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal0 ++ inReal1
          len = fromIntegral $ length inReal0
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- NATR                 Normalized Average True Range

foreign import ccall unsafe "ta_func.h TA_NATR"
  c_ta_natr :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_natr :: [Double] -> [Double] -> [Double] -> Int -> IO (Either Int TaOutput)
ta_natr inHigh inLow inClose optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_natr startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- OBV                  On Balance Volume

foreign import ccall unsafe "ta_func.h TA_OBV"
  c_ta_obv :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_obv :: [Double] -> [Double] -> IO (Either Int TaOutput)
ta_obv inReal inVolume1
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_obv startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal ++ inVolume1
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- PLUS_DI              Plus Directional Indicator

foreign import ccall unsafe "ta_func.h TA_PLUS_DI"
  c_ta_plus_di :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_plus_di :: [Double] -> [Double] -> [Double] -> Int -> IO (Either Int TaOutput)
ta_plus_di inHigh inLow inClose optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_plus_di startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- PLUS_DM              Plus Directional Movement

foreign import ccall unsafe "ta_func.h TA_PLUS_DM"
  c_ta_plus_dm :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_plus_dm :: [Double] -> [Double] -> Int -> IO (Either Int TaOutput)
ta_plus_dm inHigh inLow optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_plus_dm startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- PPO                  Percentage Price Oscillator

foreign import ccall unsafe "ta_func.h TA_PPO"
  c_ta_ppo :: CInt -> CInt -> Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_ppo :: [Double] -> Int -> Int -> Int -> IO (Either Int TaOutput)
ta_ppo inReal optInFastPeriod optInSlowPeriod optInMAType
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_ppo startIdx endIdx (getInArrPtr 0) (fromIntegral optInFastPeriod) (fromIntegral optInSlowPeriod) (fromIntegral optInMAType) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- ROC                  Rate of change : ((price/prevPrice)-1)*100

foreign import ccall unsafe "ta_func.h TA_ROC"
  c_ta_roc :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_roc :: [Double] -> Int -> IO (Either Int TaOutput)
ta_roc inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_roc startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- ROCP                 Rate of change Percentage: (price-prevPrice)/prevPrice

foreign import ccall unsafe "ta_func.h TA_ROCP"
  c_ta_rocp :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_rocp :: [Double] -> Int -> IO (Either Int TaOutput)
ta_rocp inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_rocp startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- ROCR                 Rate of change ratio: (price/prevPrice)

foreign import ccall unsafe "ta_func.h TA_ROCR"
  c_ta_rocr :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_rocr :: [Double] -> Int -> IO (Either Int TaOutput)
ta_rocr inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_rocr startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- ROCR100              Rate of change ratio 100 scale: (price/prevPrice)*100

foreign import ccall unsafe "ta_func.h TA_ROCR100"
  c_ta_rocr100 :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_rocr100 :: [Double] -> Int -> IO (Either Int TaOutput)
ta_rocr100 inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_rocr100 startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- RSI                  Relative Strength Index

foreign import ccall unsafe "ta_func.h TA_RSI"
  c_ta_rsi :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_rsi :: [Double] -> Int -> IO (Either Int TaOutput)
ta_rsi inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_rsi startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- SAR                  Parabolic SAR

foreign import ccall unsafe "ta_func.h TA_SAR"
  c_ta_sar :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_sar :: [Double] -> [Double] -> Double -> Double -> IO (Either Int TaOutput)
ta_sar inHigh inLow optInAcceleration optInMaximum
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_sar startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (realToFrac optInAcceleration) (realToFrac optInMaximum) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- SAREXT               Parabolic SAR - Extended

foreign import ccall unsafe "ta_func.h TA_SAREXT"
  c_ta_sarext :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_sarext :: [Double] -> [Double] -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO (Either Int TaOutput)
ta_sarext inHigh inLow optInStartValue optInOffsetOnReverse optInAccelerationInitLong optInAccelerationLong optInAccelerationMaxLong optInAccelerationInitShort optInAccelerationShort optInAccelerationMaxShort
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_sarext startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (realToFrac optInStartValue) (realToFrac optInOffsetOnReverse) (realToFrac optInAccelerationInitLong) (realToFrac optInAccelerationLong) (realToFrac optInAccelerationMaxLong) (realToFrac optInAccelerationInitShort) (realToFrac optInAccelerationShort) (realToFrac optInAccelerationMaxShort) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- SIN                  Vector Trigonometric Sin

foreign import ccall unsafe "ta_func.h TA_SIN"
  c_ta_sin :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_sin :: [Double] -> IO (Either Int TaOutput)
ta_sin inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_sin startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- SINH                 Vector Trigonometric Sinh

foreign import ccall unsafe "ta_func.h TA_SINH"
  c_ta_sinh :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_sinh :: [Double] -> IO (Either Int TaOutput)
ta_sinh inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_sinh startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- SMA                  Simple Moving Average

foreign import ccall unsafe "ta_func.h TA_SMA"
  c_ta_sma :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_sma :: [Double] -> Int -> IO (Either Int TaOutput)
ta_sma inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_sma startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- SQRT                 Vector Square Root

foreign import ccall unsafe "ta_func.h TA_SQRT"
  c_ta_sqrt :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_sqrt :: [Double] -> IO (Either Int TaOutput)
ta_sqrt inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_sqrt startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- STDDEV               Standard Deviation

foreign import ccall unsafe "ta_func.h TA_STDDEV"
  c_ta_stddev :: CInt -> CInt -> Ptr CDouble -> CInt -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_stddev :: [Double] -> Int -> Double -> IO (Either Int TaOutput)
ta_stddev inReal optInTimePeriod optInNbDev
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_stddev startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) (realToFrac optInNbDev) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- STOCH                Stochastic

foreign import ccall unsafe "ta_func.h TA_STOCH"
  c_ta_stoch :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_stoch :: [Double] -> [Double] -> [Double] -> Int -> Int -> Int -> Int -> Int -> IO (Either Int TaOutput)
ta_stoch inHigh inLow inClose optInFastK_Period optInSlowK_Period optInSlowK_MAType optInSlowD_Period optInSlowD_MAType
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_stoch startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInFastK_Period) (fromIntegral optInSlowK_Period) (fromIntegral optInSlowK_MAType) (fromIntegral optInSlowD_Period) (fromIntegral optInSlowD_MAType) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 2

-- STOCHF               Stochastic Fast

foreign import ccall unsafe "ta_func.h TA_STOCHF"
  c_ta_stochf :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_stochf :: [Double] -> [Double] -> [Double] -> Int -> Int -> Int -> IO (Either Int TaOutput)
ta_stochf inHigh inLow inClose optInFastK_Period optInFastD_Period optInFastD_MAType
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_stochf startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInFastK_Period) (fromIntegral optInFastD_Period) (fromIntegral optInFastD_MAType) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 2

-- STOCHRSI             Stochastic Relative Strength Index

foreign import ccall unsafe "ta_func.h TA_STOCHRSI"
  c_ta_stochrsi :: CInt -> CInt -> Ptr CDouble -> CInt -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_stochrsi :: [Double] -> Int -> Int -> Int -> Int -> IO (Either Int TaOutput)
ta_stochrsi inReal optInTimePeriod optInFastK_Period optInFastD_Period optInFastD_MAType
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_stochrsi startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) (fromIntegral optInFastK_Period) (fromIntegral optInFastD_Period) (fromIntegral optInFastD_MAType) cOutBegIdx cOutNbElement (getOutArrPtr 0) (getOutArrPtr 1)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 2

-- SUB                  Vector Arithmetic Substraction

foreign import ccall unsafe "ta_func.h TA_SUB"
  c_ta_sub :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_sub :: [Double] -> [Double] -> IO (Either Int TaOutput)
ta_sub inReal0 inReal1
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_sub startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal0 ++ inReal1
          len = fromIntegral $ length inReal0
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- SUM                  Summation

foreign import ccall unsafe "ta_func.h TA_SUM"
  c_ta_sum :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_sum :: [Double] -> Int -> IO (Either Int TaOutput)
ta_sum inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_sum startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- T3                   Triple Exponential Moving Average (T3)

foreign import ccall unsafe "ta_func.h TA_T3"
  c_ta_t3 :: CInt -> CInt -> Ptr CDouble -> CInt -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_t3 :: [Double] -> Int -> Double -> IO (Either Int TaOutput)
ta_t3 inReal optInTimePeriod optInVFactor
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_t3 startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) (realToFrac optInVFactor) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- TAN                  Vector Trigonometric Tan

foreign import ccall unsafe "ta_func.h TA_TAN"
  c_ta_tan :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_tan :: [Double] -> IO (Either Int TaOutput)
ta_tan inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_tan startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- TANH                 Vector Trigonometric Tanh

foreign import ccall unsafe "ta_func.h TA_TANH"
  c_ta_tanh :: CInt -> CInt -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_tanh :: [Double] -> IO (Either Int TaOutput)
ta_tanh inReal
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_tanh startIdx endIdx (getInArrPtr 0) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- TEMA                 Triple Exponential Moving Average

foreign import ccall unsafe "ta_func.h TA_TEMA"
  c_ta_tema :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_tema :: [Double] -> Int -> IO (Either Int TaOutput)
ta_tema inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_tema startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- TRANGE               True Range

foreign import ccall unsafe "ta_func.h TA_TRANGE"
  c_ta_trange :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_trange :: [Double] -> [Double] -> [Double] -> IO (Either Int TaOutput)
ta_trange inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_trange startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- TRIMA                Triangular Moving Average

foreign import ccall unsafe "ta_func.h TA_TRIMA"
  c_ta_trima :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_trima :: [Double] -> Int -> IO (Either Int TaOutput)
ta_trima inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_trima startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- TRIX                 1-day Rate-Of-Change (ROC) of a Triple Smooth EMA

foreign import ccall unsafe "ta_func.h TA_TRIX"
  c_ta_trix :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_trix :: [Double] -> Int -> IO (Either Int TaOutput)
ta_trix inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_trix startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- TSF                  Time Series Forecast

foreign import ccall unsafe "ta_func.h TA_TSF"
  c_ta_tsf :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_tsf :: [Double] -> Int -> IO (Either Int TaOutput)
ta_tsf inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_tsf startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- TYPPRICE             Typical Price

foreign import ccall unsafe "ta_func.h TA_TYPPRICE"
  c_ta_typprice :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_typprice :: [Double] -> [Double] -> [Double] -> IO (Either Int TaOutput)
ta_typprice inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_typprice startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- ULTOSC               Ultimate Oscillator

foreign import ccall unsafe "ta_func.h TA_ULTOSC"
  c_ta_ultosc :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_ultosc :: [Double] -> [Double] -> [Double] -> Int -> Int -> Int -> IO (Either Int TaOutput)
ta_ultosc inHigh inLow inClose optInTimePeriod1 optInTimePeriod2 optInTimePeriod3
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_ultosc startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInTimePeriod1) (fromIntegral optInTimePeriod2) (fromIntegral optInTimePeriod3) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- VAR                  Variance

foreign import ccall unsafe "ta_func.h TA_VAR"
  c_ta_var :: CInt -> CInt -> Ptr CDouble -> CInt -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_var :: [Double] -> Int -> Double -> IO (Either Int TaOutput)
ta_var inReal optInTimePeriod optInNbDev
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_var startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) (realToFrac optInNbDev) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- WCLPRICE             Weighted Close Price

foreign import ccall unsafe "ta_func.h TA_WCLPRICE"
  c_ta_wclprice :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_wclprice :: [Double] -> [Double] -> [Double] -> IO (Either Int TaOutput)
ta_wclprice inHigh inLow inClose
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_wclprice startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- WILLR                Williams' %R

foreign import ccall unsafe "ta_func.h TA_WILLR"
  c_ta_willr :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_willr :: [Double] -> [Double] -> [Double] -> Int -> IO (Either Int TaOutput)
ta_willr inHigh inLow inClose optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_willr startIdx endIdx (getInArrPtr 0) (getInArrPtr 1) (getInArrPtr 2) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inHigh ++ inLow ++ inClose
          len = fromIntegral $ length inHigh
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

-- WMA                  Weighted Moving Average

foreign import ccall unsafe "ta_func.h TA_WMA"
  c_ta_wma :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

ta_wma :: [Double] -> Int -> IO (Either Int TaOutput)
ta_wma inReal optInTimePeriod
    = withArray _inReal            $ \cInReal ->
      alloca                       $ \cOutBegIdx ->
      alloca                       $ \cOutNbElement ->
      allocaArray (len * outputs)  $ \cOutReal ->
      let getArrPtr i arr = plusPtr arr (i * (sizeOf arr) * len)
          getInArrPtr i   = getArrPtr i cInReal
          getOutArrPtr i  = getArrPtr i cOutReal
      in do
        rc <- c_ta_wma startIdx endIdx (getInArrPtr 0) (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement (getOutArrPtr 0)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx = fromIntegral outBegIdx,
                                           outNBElement = fromIntegral outNbElement,
                                           outCount = outputs,
                                           out = chunksOf len outReal
                                         }
          _ -> return $ Left $ fromIntegral rc
    where _inReal = inReal
          len = fromIntegral $ length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

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
        
        
        
        
        
        
        
        