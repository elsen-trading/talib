module Data.TALib where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Storable
import System.IO.Unsafe
import Data.List.Split
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

-- TODO: C functions return an enum, not a CInt

taIntDefault = fromIntegral (minBound :: CInt)

-- TA-Lib : Technical Analysis Library

foreign import ccall unsafe "ta_common.h TA_Initialize"
  c_ta_init :: IO ()
               
vecPtr :: VM.Storable a => VM.MVector s a -> ForeignPtr a
vecPtr = fst . VM.unsafeToForeignPtr0

-- *********************************
-- *** Start Auto-Generated Code ***
-- *********************************

--
-- ACOS                 Vector Trigonometric ACos
--

foreign import ccall unsafe "ta_func.h TA_ACOS"
  c_ta_acos :: CInt -> CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_acos :: V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_acos inReal = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_acos 0 (fromIntegral $ len - 1) c_inReal cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- AD                   Chaikin A/D Line
--

foreign import ccall unsafe "ta_func.h TA_AD"
  c_ta_ad :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
--   inVolume
-- arguments
-- outputs
--   outReal (double[])

ta_ad :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_ad inHigh inLow inClose inVolume = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _inVolume <- V.unsafeThaw inVolume
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        withForeignPtr (vecPtr _inClose) $ \c_inClose ->
          withForeignPtr (vecPtr _inVolume) $ \c_inVolume ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outReal) $ \c_outReal ->
                  do rc <- c_ta_ad 0 (fromIntegral $ len - 1) c_inHigh c_inLow c_inClose c_inVolume cOutBegIdx cOutNbElement c_outReal
                     out_outReal <- V.unsafeFreeze _outReal
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outReal)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- ADD                  Vector Arithmetic Add
--

foreign import ccall unsafe "ta_func.h TA_ADD"
  c_ta_add :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal0
--   inReal1
-- arguments
-- outputs
--   outReal (double[])

ta_add :: V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_add inReal0 inReal1 = do
    _inReal0 <- V.unsafeThaw inReal0
    _inReal1 <- V.unsafeThaw inReal1
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal0) $ \c_inReal0 ->
      withForeignPtr (vecPtr _inReal1) $ \c_inReal1 ->
        alloca $ \cOutBegIdx ->
          alloca $ \cOutNbElement ->
            withForeignPtr (vecPtr _outReal) $ \c_outReal ->
              do rc <- c_ta_add 0 (fromIntegral $ len - 1) c_inReal0 c_inReal1 cOutBegIdx cOutNbElement c_outReal
                 out_outReal <- V.unsafeFreeze _outReal
                 case rc of
                   0 -> do outBegIdx <- peek cOutBegIdx
                           outNbElement <- peek cOutNbElement
                           return $ Right $ (fromIntegral outBegIdx,
                                             fromIntegral outNbElement,
                                             out_outReal)
                   _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal0

--
-- ADOSC                Chaikin A/D Oscillator
--

foreign import ccall unsafe "ta_func.h TA_ADOSC"
  c_ta_adosc :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

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

ta_adosc :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> Int -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_adosc inHigh inLow inClose inVolume optInFastPeriod optInSlowPeriod = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _inVolume <- V.unsafeThaw inVolume
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        withForeignPtr (vecPtr _inClose) $ \c_inClose ->
          withForeignPtr (vecPtr _inVolume) $ \c_inVolume ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outReal) $ \c_outReal ->
                  do rc <- c_ta_adosc 0 (fromIntegral $ len - 1) c_inHigh c_inLow c_inClose c_inVolume (fromIntegral optInFastPeriod) (fromIntegral optInSlowPeriod) cOutBegIdx cOutNbElement c_outReal
                     out_outReal <- V.unsafeFreeze _outReal
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outReal)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- ADX                  Average Directional Movement Index
--

foreign import ccall unsafe "ta_func.h TA_ADX"
  c_ta_adx :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_adx :: V.Vector Double -> V.Vector Double -> V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_adx inHigh inLow inClose optInTimePeriod = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        withForeignPtr (vecPtr _inClose) $ \c_inClose ->
          alloca $ \cOutBegIdx ->
            alloca $ \cOutNbElement ->
              withForeignPtr (vecPtr _outReal) $ \c_outReal ->
                do rc <- c_ta_adx 0 (fromIntegral $ len - 1) c_inHigh c_inLow c_inClose (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
                   out_outReal <- V.unsafeFreeze _outReal
                   case rc of
                     0 -> do outBegIdx <- peek cOutBegIdx
                             outNbElement <- peek cOutNbElement
                             return $ Right $ (fromIntegral outBegIdx,
                                               fromIntegral outNbElement,
                                               out_outReal)
                     _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- ADXR                 Average Directional Movement Index Rating
--

foreign import ccall unsafe "ta_func.h TA_ADXR"
  c_ta_adxr :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_adxr :: V.Vector Double -> V.Vector Double -> V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_adxr inHigh inLow inClose optInTimePeriod = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        withForeignPtr (vecPtr _inClose) $ \c_inClose ->
          alloca $ \cOutBegIdx ->
            alloca $ \cOutNbElement ->
              withForeignPtr (vecPtr _outReal) $ \c_outReal ->
                do rc <- c_ta_adxr 0 (fromIntegral $ len - 1) c_inHigh c_inLow c_inClose (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
                   out_outReal <- V.unsafeFreeze _outReal
                   case rc of
                     0 -> do outBegIdx <- peek cOutBegIdx
                             outNbElement <- peek cOutNbElement
                             return $ Right $ (fromIntegral outBegIdx,
                                               fromIntegral outNbElement,
                                               out_outReal)
                     _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- APO                  Absolute Price Oscillator
--

foreign import ccall unsafe "ta_func.h TA_APO"
  c_ta_apo :: CInt -> CInt -> Ptr Double -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInFastPeriod (int)
--   optInSlowPeriod (int)
--   optInMAType (int)
-- outputs
--   outReal (double[])

ta_apo :: V.Vector Double -> Int -> Int -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_apo inReal optInFastPeriod optInSlowPeriod optInMAType = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_apo 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInFastPeriod) (fromIntegral optInSlowPeriod) (fromIntegral optInMAType) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- AROON                Aroon
--

foreign import ccall unsafe "ta_func.h TA_AROON"
  c_ta_aroon :: CInt -> CInt -> Ptr Double -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> Ptr Double -> IO CInt

-- inputs
--   inHigh
--   inLow
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outAroonDown (double[])
--   outAroonUp (double[])

ta_aroon :: V.Vector Double -> V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double, V.Vector Double))
ta_aroon inHigh inLow optInTimePeriod = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _outAroonDown <- VM.new len
    _outAroonUp <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        alloca $ \cOutBegIdx ->
          alloca $ \cOutNbElement ->
            withForeignPtr (vecPtr _outAroonDown) $ \c_outAroonDown ->
              withForeignPtr (vecPtr _outAroonUp) $ \c_outAroonUp ->
                do rc <- c_ta_aroon 0 (fromIntegral $ len - 1) c_inHigh c_inLow (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outAroonDown c_outAroonUp
                   out_outAroonDown <- V.unsafeFreeze _outAroonDown
                   out_outAroonUp <- V.unsafeFreeze _outAroonUp
                   case rc of
                     0 -> do outBegIdx <- peek cOutBegIdx
                             outNbElement <- peek cOutNbElement
                             return $ Right $ (fromIntegral outBegIdx,
                                               fromIntegral outNbElement,
                                               out_outAroonDown,
                                               out_outAroonUp)
                     _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- AROONOSC             Aroon Oscillator
--

foreign import ccall unsafe "ta_func.h TA_AROONOSC"
  c_ta_aroonosc :: CInt -> CInt -> Ptr Double -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inHigh
--   inLow
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_aroonosc :: V.Vector Double -> V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_aroonosc inHigh inLow optInTimePeriod = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        alloca $ \cOutBegIdx ->
          alloca $ \cOutNbElement ->
            withForeignPtr (vecPtr _outReal) $ \c_outReal ->
              do rc <- c_ta_aroonosc 0 (fromIntegral $ len - 1) c_inHigh c_inLow (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
                 out_outReal <- V.unsafeFreeze _outReal
                 case rc of
                   0 -> do outBegIdx <- peek cOutBegIdx
                           outNbElement <- peek cOutNbElement
                           return $ Right $ (fromIntegral outBegIdx,
                                             fromIntegral outNbElement,
                                             out_outReal)
                   _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- ASIN                 Vector Trigonometric ASin
--

foreign import ccall unsafe "ta_func.h TA_ASIN"
  c_ta_asin :: CInt -> CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_asin :: V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_asin inReal = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_asin 0 (fromIntegral $ len - 1) c_inReal cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- ATAN                 Vector Trigonometric ATan
--

foreign import ccall unsafe "ta_func.h TA_ATAN"
  c_ta_atan :: CInt -> CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_atan :: V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_atan inReal = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_atan 0 (fromIntegral $ len - 1) c_inReal cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- ATR                  Average True Range
--

foreign import ccall unsafe "ta_func.h TA_ATR"
  c_ta_atr :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_atr :: V.Vector Double -> V.Vector Double -> V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_atr inHigh inLow inClose optInTimePeriod = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        withForeignPtr (vecPtr _inClose) $ \c_inClose ->
          alloca $ \cOutBegIdx ->
            alloca $ \cOutNbElement ->
              withForeignPtr (vecPtr _outReal) $ \c_outReal ->
                do rc <- c_ta_atr 0 (fromIntegral $ len - 1) c_inHigh c_inLow c_inClose (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
                   out_outReal <- V.unsafeFreeze _outReal
                   case rc of
                     0 -> do outBegIdx <- peek cOutBegIdx
                             outNbElement <- peek cOutNbElement
                             return $ Right $ (fromIntegral outBegIdx,
                                               fromIntegral outNbElement,
                                               out_outReal)
                     _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- AVGPRICE             Average Price
--

foreign import ccall unsafe "ta_func.h TA_AVGPRICE"
  c_ta_avgprice :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outReal (double[])

ta_avgprice :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_avgprice inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outReal) $ \c_outReal ->
                  do rc <- c_ta_avgprice 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outReal
                     out_outReal <- V.unsafeFreeze _outReal
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outReal)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- BBANDS               Bollinger Bands
--

foreign import ccall unsafe "ta_func.h TA_BBANDS"
  c_ta_bbands :: CInt -> CInt -> Ptr Double -> CInt -> CDouble -> CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> Ptr Double -> Ptr Double -> IO CInt

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

ta_bbands :: V.Vector Double -> Int -> Double -> Double -> Int -> IO (Either Int (Int, Int, V.Vector Double, V.Vector Double, V.Vector Double))
ta_bbands inReal optInTimePeriod optInNbDevUp optInNbDevDn optInMAType = do
    _inReal <- V.unsafeThaw inReal
    _outRealUpperBand <- VM.new len
    _outRealMiddleBand <- VM.new len
    _outRealLowerBand <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outRealUpperBand) $ \c_outRealUpperBand ->
            withForeignPtr (vecPtr _outRealMiddleBand) $ \c_outRealMiddleBand ->
              withForeignPtr (vecPtr _outRealLowerBand) $ \c_outRealLowerBand ->
                do rc <- c_ta_bbands 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) (realToFrac optInNbDevUp) (realToFrac optInNbDevDn) (fromIntegral optInMAType) cOutBegIdx cOutNbElement c_outRealUpperBand c_outRealMiddleBand c_outRealLowerBand
                   out_outRealUpperBand <- V.unsafeFreeze _outRealUpperBand
                   out_outRealMiddleBand <- V.unsafeFreeze _outRealMiddleBand
                   out_outRealLowerBand <- V.unsafeFreeze _outRealLowerBand
                   case rc of
                     0 -> do outBegIdx <- peek cOutBegIdx
                             outNbElement <- peek cOutNbElement
                             return $ Right $ (fromIntegral outBegIdx,
                                               fromIntegral outNbElement,
                                               out_outRealUpperBand,
                                               out_outRealMiddleBand,
                                               out_outRealLowerBand)
                     _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- BETA                 Beta
--

foreign import ccall unsafe "ta_func.h TA_BETA"
  c_ta_beta :: CInt -> CInt -> Ptr Double -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal0
--   inReal1
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_beta :: V.Vector Double -> V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_beta inReal0 inReal1 optInTimePeriod = do
    _inReal0 <- V.unsafeThaw inReal0
    _inReal1 <- V.unsafeThaw inReal1
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal0) $ \c_inReal0 ->
      withForeignPtr (vecPtr _inReal1) $ \c_inReal1 ->
        alloca $ \cOutBegIdx ->
          alloca $ \cOutNbElement ->
            withForeignPtr (vecPtr _outReal) $ \c_outReal ->
              do rc <- c_ta_beta 0 (fromIntegral $ len - 1) c_inReal0 c_inReal1 (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
                 out_outReal <- V.unsafeFreeze _outReal
                 case rc of
                   0 -> do outBegIdx <- peek cOutBegIdx
                           outNbElement <- peek cOutNbElement
                           return $ Right $ (fromIntegral outBegIdx,
                                             fromIntegral outNbElement,
                                             out_outReal)
                   _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal0

--
-- BOP                  Balance Of Power
--

foreign import ccall unsafe "ta_func.h TA_BOP"
  c_ta_bop :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outReal (double[])

ta_bop :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_bop inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outReal) $ \c_outReal ->
                  do rc <- c_ta_bop 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outReal
                     out_outReal <- V.unsafeFreeze _outReal
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outReal)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CCI                  Commodity Channel Index
--

foreign import ccall unsafe "ta_func.h TA_CCI"
  c_ta_cci :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_cci :: V.Vector Double -> V.Vector Double -> V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_cci inHigh inLow inClose optInTimePeriod = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        withForeignPtr (vecPtr _inClose) $ \c_inClose ->
          alloca $ \cOutBegIdx ->
            alloca $ \cOutNbElement ->
              withForeignPtr (vecPtr _outReal) $ \c_outReal ->
                do rc <- c_ta_cci 0 (fromIntegral $ len - 1) c_inHigh c_inLow c_inClose (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
                   out_outReal <- V.unsafeFreeze _outReal
                   case rc of
                     0 -> do outBegIdx <- peek cOutBegIdx
                             outNbElement <- peek cOutNbElement
                             return $ Right $ (fromIntegral outBegIdx,
                                               fromIntegral outNbElement,
                                               out_outReal)
                     _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- CDL2CROWS            Two Crows
--

foreign import ccall unsafe "ta_func.h TA_CDL2CROWS"
  c_ta_cdl2crows :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdl2crows :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdl2crows inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdl2crows 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDL3BLACKCROWS       Three Black Crows
--

foreign import ccall unsafe "ta_func.h TA_CDL3BLACKCROWS"
  c_ta_cdl3blackcrows :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdl3blackcrows :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdl3blackcrows inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdl3blackcrows 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDL3INSIDE           Three Inside Up/Down
--

foreign import ccall unsafe "ta_func.h TA_CDL3INSIDE"
  c_ta_cdl3inside :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdl3inside :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdl3inside inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdl3inside 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDL3LINESTRIKE       Three-Line Strike 
--

foreign import ccall unsafe "ta_func.h TA_CDL3LINESTRIKE"
  c_ta_cdl3linestrike :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdl3linestrike :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdl3linestrike inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdl3linestrike 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDL3OUTSIDE          Three Outside Up/Down
--

foreign import ccall unsafe "ta_func.h TA_CDL3OUTSIDE"
  c_ta_cdl3outside :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdl3outside :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdl3outside inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdl3outside 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDL3STARSINSOUTH     Three Stars In The South
--

foreign import ccall unsafe "ta_func.h TA_CDL3STARSINSOUTH"
  c_ta_cdl3starsinsouth :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdl3starsinsouth :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdl3starsinsouth inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdl3starsinsouth 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDL3WHITESOLDIERS    Three Advancing White Soldiers
--

foreign import ccall unsafe "ta_func.h TA_CDL3WHITESOLDIERS"
  c_ta_cdl3whitesoldiers :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdl3whitesoldiers :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdl3whitesoldiers inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdl3whitesoldiers 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLABANDONEDBABY     Abandoned Baby
--

foreign import ccall unsafe "ta_func.h TA_CDLABANDONEDBABY"
  c_ta_cdlabandonedbaby :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInPenetration (double)
-- outputs
--   outInteger (int[])

ta_cdlabandonedbaby :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlabandonedbaby inOpen inHigh inLow inClose optInPenetration = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlabandonedbaby 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose (realToFrac optInPenetration) cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLADVANCEBLOCK      Advance Block
--

foreign import ccall unsafe "ta_func.h TA_CDLADVANCEBLOCK"
  c_ta_cdladvanceblock :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdladvanceblock :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdladvanceblock inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdladvanceblock 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLBELTHOLD          Belt-hold
--

foreign import ccall unsafe "ta_func.h TA_CDLBELTHOLD"
  c_ta_cdlbelthold :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlbelthold :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlbelthold inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlbelthold 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLBREAKAWAY         Breakaway
--

foreign import ccall unsafe "ta_func.h TA_CDLBREAKAWAY"
  c_ta_cdlbreakaway :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlbreakaway :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlbreakaway inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlbreakaway 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLCLOSINGMARUBOZU   Closing Marubozu
--

foreign import ccall unsafe "ta_func.h TA_CDLCLOSINGMARUBOZU"
  c_ta_cdlclosingmarubozu :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlclosingmarubozu :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlclosingmarubozu inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlclosingmarubozu 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLCONCEALBABYSWALL  Concealing Baby Swallow
--

foreign import ccall unsafe "ta_func.h TA_CDLCONCEALBABYSWALL"
  c_ta_cdlconcealbabyswall :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlconcealbabyswall :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlconcealbabyswall inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlconcealbabyswall 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLCOUNTERATTACK     Counterattack
--

foreign import ccall unsafe "ta_func.h TA_CDLCOUNTERATTACK"
  c_ta_cdlcounterattack :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlcounterattack :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlcounterattack inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlcounterattack 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLDARKCLOUDCOVER    Dark Cloud Cover
--

foreign import ccall unsafe "ta_func.h TA_CDLDARKCLOUDCOVER"
  c_ta_cdldarkcloudcover :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInPenetration (double)
-- outputs
--   outInteger (int[])

ta_cdldarkcloudcover :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdldarkcloudcover inOpen inHigh inLow inClose optInPenetration = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdldarkcloudcover 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose (realToFrac optInPenetration) cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLDOJI              Doji
--

foreign import ccall unsafe "ta_func.h TA_CDLDOJI"
  c_ta_cdldoji :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdldoji :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdldoji inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdldoji 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLDOJISTAR          Doji Star
--

foreign import ccall unsafe "ta_func.h TA_CDLDOJISTAR"
  c_ta_cdldojistar :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdldojistar :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdldojistar inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdldojistar 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLDRAGONFLYDOJI     Dragonfly Doji
--

foreign import ccall unsafe "ta_func.h TA_CDLDRAGONFLYDOJI"
  c_ta_cdldragonflydoji :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdldragonflydoji :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdldragonflydoji inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdldragonflydoji 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLENGULFING         Engulfing Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLENGULFING"
  c_ta_cdlengulfing :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlengulfing :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlengulfing inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlengulfing 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLEVENINGDOJISTAR   Evening Doji Star
--

foreign import ccall unsafe "ta_func.h TA_CDLEVENINGDOJISTAR"
  c_ta_cdleveningdojistar :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInPenetration (double)
-- outputs
--   outInteger (int[])

ta_cdleveningdojistar :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdleveningdojistar inOpen inHigh inLow inClose optInPenetration = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdleveningdojistar 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose (realToFrac optInPenetration) cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLEVENINGSTAR       Evening Star
--

foreign import ccall unsafe "ta_func.h TA_CDLEVENINGSTAR"
  c_ta_cdleveningstar :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInPenetration (double)
-- outputs
--   outInteger (int[])

ta_cdleveningstar :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdleveningstar inOpen inHigh inLow inClose optInPenetration = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdleveningstar 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose (realToFrac optInPenetration) cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLGAPSIDESIDEWHITE  Up/Down-gap side-by-side white lines
--

foreign import ccall unsafe "ta_func.h TA_CDLGAPSIDESIDEWHITE"
  c_ta_cdlgapsidesidewhite :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlgapsidesidewhite :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlgapsidesidewhite inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlgapsidesidewhite 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLGRAVESTONEDOJI    Gravestone Doji
--

foreign import ccall unsafe "ta_func.h TA_CDLGRAVESTONEDOJI"
  c_ta_cdlgravestonedoji :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlgravestonedoji :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlgravestonedoji inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlgravestonedoji 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLHAMMER            Hammer
--

foreign import ccall unsafe "ta_func.h TA_CDLHAMMER"
  c_ta_cdlhammer :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlhammer :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlhammer inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlhammer 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLHANGINGMAN        Hanging Man
--

foreign import ccall unsafe "ta_func.h TA_CDLHANGINGMAN"
  c_ta_cdlhangingman :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlhangingman :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlhangingman inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlhangingman 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLHARAMI            Harami Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLHARAMI"
  c_ta_cdlharami :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlharami :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlharami inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlharami 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLHARAMICROSS       Harami Cross Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLHARAMICROSS"
  c_ta_cdlharamicross :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlharamicross :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlharamicross inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlharamicross 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLHIGHWAVE          High-Wave Candle
--

foreign import ccall unsafe "ta_func.h TA_CDLHIGHWAVE"
  c_ta_cdlhighwave :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlhighwave :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlhighwave inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlhighwave 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLHIKKAKE           Hikkake Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLHIKKAKE"
  c_ta_cdlhikkake :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlhikkake :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlhikkake inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlhikkake 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLHIKKAKEMOD        Modified Hikkake Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLHIKKAKEMOD"
  c_ta_cdlhikkakemod :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlhikkakemod :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlhikkakemod inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlhikkakemod 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLHOMINGPIGEON      Homing Pigeon
--

foreign import ccall unsafe "ta_func.h TA_CDLHOMINGPIGEON"
  c_ta_cdlhomingpigeon :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlhomingpigeon :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlhomingpigeon inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlhomingpigeon 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLIDENTICAL3CROWS   Identical Three Crows
--

foreign import ccall unsafe "ta_func.h TA_CDLIDENTICAL3CROWS"
  c_ta_cdlidentical3crows :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlidentical3crows :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlidentical3crows inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlidentical3crows 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLINNECK            In-Neck Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLINNECK"
  c_ta_cdlinneck :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlinneck :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlinneck inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlinneck 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLINVERTEDHAMMER    Inverted Hammer
--

foreign import ccall unsafe "ta_func.h TA_CDLINVERTEDHAMMER"
  c_ta_cdlinvertedhammer :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlinvertedhammer :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlinvertedhammer inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlinvertedhammer 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLKICKING           Kicking
--

foreign import ccall unsafe "ta_func.h TA_CDLKICKING"
  c_ta_cdlkicking :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlkicking :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlkicking inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlkicking 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLKICKINGBYLENGTH   Kicking - bull/bear determined by the longer marubozu
--

foreign import ccall unsafe "ta_func.h TA_CDLKICKINGBYLENGTH"
  c_ta_cdlkickingbylength :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlkickingbylength :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlkickingbylength inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlkickingbylength 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLLADDERBOTTOM      Ladder Bottom
--

foreign import ccall unsafe "ta_func.h TA_CDLLADDERBOTTOM"
  c_ta_cdlladderbottom :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlladderbottom :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlladderbottom inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlladderbottom 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLLONGLEGGEDDOJI    Long Legged Doji
--

foreign import ccall unsafe "ta_func.h TA_CDLLONGLEGGEDDOJI"
  c_ta_cdllongleggeddoji :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdllongleggeddoji :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdllongleggeddoji inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdllongleggeddoji 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLLONGLINE          Long Line Candle
--

foreign import ccall unsafe "ta_func.h TA_CDLLONGLINE"
  c_ta_cdllongline :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdllongline :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdllongline inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdllongline 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLMARUBOZU          Marubozu
--

foreign import ccall unsafe "ta_func.h TA_CDLMARUBOZU"
  c_ta_cdlmarubozu :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlmarubozu :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlmarubozu inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlmarubozu 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLMATCHINGLOW       Matching Low
--

foreign import ccall unsafe "ta_func.h TA_CDLMATCHINGLOW"
  c_ta_cdlmatchinglow :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlmatchinglow :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlmatchinglow inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlmatchinglow 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLMATHOLD           Mat Hold
--

foreign import ccall unsafe "ta_func.h TA_CDLMATHOLD"
  c_ta_cdlmathold :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInPenetration (double)
-- outputs
--   outInteger (int[])

ta_cdlmathold :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlmathold inOpen inHigh inLow inClose optInPenetration = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlmathold 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose (realToFrac optInPenetration) cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLMORNINGDOJISTAR   Morning Doji Star
--

foreign import ccall unsafe "ta_func.h TA_CDLMORNINGDOJISTAR"
  c_ta_cdlmorningdojistar :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInPenetration (double)
-- outputs
--   outInteger (int[])

ta_cdlmorningdojistar :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlmorningdojistar inOpen inHigh inLow inClose optInPenetration = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlmorningdojistar 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose (realToFrac optInPenetration) cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLMORNINGSTAR       Morning Star
--

foreign import ccall unsafe "ta_func.h TA_CDLMORNINGSTAR"
  c_ta_cdlmorningstar :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInPenetration (double)
-- outputs
--   outInteger (int[])

ta_cdlmorningstar :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlmorningstar inOpen inHigh inLow inClose optInPenetration = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlmorningstar 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose (realToFrac optInPenetration) cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLONNECK            On-Neck Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLONNECK"
  c_ta_cdlonneck :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlonneck :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlonneck inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlonneck 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLPIERCING          Piercing Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLPIERCING"
  c_ta_cdlpiercing :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlpiercing :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlpiercing inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlpiercing 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLRICKSHAWMAN       Rickshaw Man
--

foreign import ccall unsafe "ta_func.h TA_CDLRICKSHAWMAN"
  c_ta_cdlrickshawman :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlrickshawman :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlrickshawman inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlrickshawman 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLRISEFALL3METHODS  Rising/Falling Three Methods
--

foreign import ccall unsafe "ta_func.h TA_CDLRISEFALL3METHODS"
  c_ta_cdlrisefall3methods :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlrisefall3methods :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlrisefall3methods inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlrisefall3methods 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLSEPARATINGLINES   Separating Lines
--

foreign import ccall unsafe "ta_func.h TA_CDLSEPARATINGLINES"
  c_ta_cdlseparatinglines :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlseparatinglines :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlseparatinglines inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlseparatinglines 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLSHOOTINGSTAR      Shooting Star
--

foreign import ccall unsafe "ta_func.h TA_CDLSHOOTINGSTAR"
  c_ta_cdlshootingstar :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlshootingstar :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlshootingstar inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlshootingstar 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLSHORTLINE         Short Line Candle
--

foreign import ccall unsafe "ta_func.h TA_CDLSHORTLINE"
  c_ta_cdlshortline :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlshortline :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlshortline inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlshortline 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLSPINNINGTOP       Spinning Top
--

foreign import ccall unsafe "ta_func.h TA_CDLSPINNINGTOP"
  c_ta_cdlspinningtop :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlspinningtop :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlspinningtop inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlspinningtop 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLSTALLEDPATTERN    Stalled Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLSTALLEDPATTERN"
  c_ta_cdlstalledpattern :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlstalledpattern :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlstalledpattern inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlstalledpattern 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLSTICKSANDWICH     Stick Sandwich
--

foreign import ccall unsafe "ta_func.h TA_CDLSTICKSANDWICH"
  c_ta_cdlsticksandwich :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlsticksandwich :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlsticksandwich inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlsticksandwich 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLTAKURI            Takuri (Dragonfly Doji with very long lower shadow)
--

foreign import ccall unsafe "ta_func.h TA_CDLTAKURI"
  c_ta_cdltakuri :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdltakuri :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdltakuri inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdltakuri 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLTASUKIGAP         Tasuki Gap
--

foreign import ccall unsafe "ta_func.h TA_CDLTASUKIGAP"
  c_ta_cdltasukigap :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdltasukigap :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdltasukigap inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdltasukigap 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLTHRUSTING         Thrusting Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLTHRUSTING"
  c_ta_cdlthrusting :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlthrusting :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlthrusting inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlthrusting 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLTRISTAR           Tristar Pattern
--

foreign import ccall unsafe "ta_func.h TA_CDLTRISTAR"
  c_ta_cdltristar :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdltristar :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdltristar inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdltristar 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLUNIQUE3RIVER      Unique 3 River
--

foreign import ccall unsafe "ta_func.h TA_CDLUNIQUE3RIVER"
  c_ta_cdlunique3river :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlunique3river :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlunique3river inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlunique3river 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLUPSIDEGAP2CROWS   Upside Gap Two Crows
--

foreign import ccall unsafe "ta_func.h TA_CDLUPSIDEGAP2CROWS"
  c_ta_cdlupsidegap2crows :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlupsidegap2crows :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlupsidegap2crows inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlupsidegap2crows 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CDLXSIDEGAP3METHODS  Upside/Downside Gap Three Methods
--

foreign import ccall unsafe "ta_func.h TA_CDLXSIDEGAP3METHODS"
  c_ta_cdlxsidegap3methods :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inOpen
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outInteger (int[])

ta_cdlxsidegap3methods :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_cdlxsidegap3methods inOpen inHigh inLow inClose = do
    _inOpen <- V.unsafeThaw inOpen
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inOpen) $ \c_inOpen ->
      withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
        withForeignPtr (vecPtr _inLow) $ \c_inLow ->
          withForeignPtr (vecPtr _inClose) $ \c_inClose ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
                  do rc <- c_ta_cdlxsidegap3methods 0 (fromIntegral $ len - 1) c_inOpen c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outInteger
                     out_outInteger <- V.unsafeFreeze _outInteger
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outInteger)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inOpen

--
-- CEIL                 Vector Ceil
--

foreign import ccall unsafe "ta_func.h TA_CEIL"
  c_ta_ceil :: CInt -> CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_ceil :: V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_ceil inReal = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_ceil 0 (fromIntegral $ len - 1) c_inReal cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- CMO                  Chande Momentum Oscillator
--

foreign import ccall unsafe "ta_func.h TA_CMO"
  c_ta_cmo :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_cmo :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_cmo inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_cmo 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- CORREL               Pearson's Correlation Coefficient (r)
--

foreign import ccall unsafe "ta_func.h TA_CORREL"
  c_ta_correl :: CInt -> CInt -> Ptr Double -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal0
--   inReal1
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_correl :: V.Vector Double -> V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_correl inReal0 inReal1 optInTimePeriod = do
    _inReal0 <- V.unsafeThaw inReal0
    _inReal1 <- V.unsafeThaw inReal1
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal0) $ \c_inReal0 ->
      withForeignPtr (vecPtr _inReal1) $ \c_inReal1 ->
        alloca $ \cOutBegIdx ->
          alloca $ \cOutNbElement ->
            withForeignPtr (vecPtr _outReal) $ \c_outReal ->
              do rc <- c_ta_correl 0 (fromIntegral $ len - 1) c_inReal0 c_inReal1 (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
                 out_outReal <- V.unsafeFreeze _outReal
                 case rc of
                   0 -> do outBegIdx <- peek cOutBegIdx
                           outNbElement <- peek cOutNbElement
                           return $ Right $ (fromIntegral outBegIdx,
                                             fromIntegral outNbElement,
                                             out_outReal)
                   _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal0

--
-- COS                  Vector Trigonometric Cos
--

foreign import ccall unsafe "ta_func.h TA_COS"
  c_ta_cos :: CInt -> CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_cos :: V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_cos inReal = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_cos 0 (fromIntegral $ len - 1) c_inReal cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- COSH                 Vector Trigonometric Cosh
--

foreign import ccall unsafe "ta_func.h TA_COSH"
  c_ta_cosh :: CInt -> CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_cosh :: V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_cosh inReal = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_cosh 0 (fromIntegral $ len - 1) c_inReal cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- DEMA                 Double Exponential Moving Average
--

foreign import ccall unsafe "ta_func.h TA_DEMA"
  c_ta_dema :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_dema :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_dema inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_dema 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- DIV                  Vector Arithmetic Div
--

foreign import ccall unsafe "ta_func.h TA_DIV"
  c_ta_div :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal0
--   inReal1
-- arguments
-- outputs
--   outReal (double[])

ta_div :: V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_div inReal0 inReal1 = do
    _inReal0 <- V.unsafeThaw inReal0
    _inReal1 <- V.unsafeThaw inReal1
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal0) $ \c_inReal0 ->
      withForeignPtr (vecPtr _inReal1) $ \c_inReal1 ->
        alloca $ \cOutBegIdx ->
          alloca $ \cOutNbElement ->
            withForeignPtr (vecPtr _outReal) $ \c_outReal ->
              do rc <- c_ta_div 0 (fromIntegral $ len - 1) c_inReal0 c_inReal1 cOutBegIdx cOutNbElement c_outReal
                 out_outReal <- V.unsafeFreeze _outReal
                 case rc of
                   0 -> do outBegIdx <- peek cOutBegIdx
                           outNbElement <- peek cOutNbElement
                           return $ Right $ (fromIntegral outBegIdx,
                                             fromIntegral outNbElement,
                                             out_outReal)
                   _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal0

--
-- DX                   Directional Movement Index
--

foreign import ccall unsafe "ta_func.h TA_DX"
  c_ta_dx :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_dx :: V.Vector Double -> V.Vector Double -> V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_dx inHigh inLow inClose optInTimePeriod = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        withForeignPtr (vecPtr _inClose) $ \c_inClose ->
          alloca $ \cOutBegIdx ->
            alloca $ \cOutNbElement ->
              withForeignPtr (vecPtr _outReal) $ \c_outReal ->
                do rc <- c_ta_dx 0 (fromIntegral $ len - 1) c_inHigh c_inLow c_inClose (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
                   out_outReal <- V.unsafeFreeze _outReal
                   case rc of
                     0 -> do outBegIdx <- peek cOutBegIdx
                             outNbElement <- peek cOutNbElement
                             return $ Right $ (fromIntegral outBegIdx,
                                               fromIntegral outNbElement,
                                               out_outReal)
                     _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- EMA                  Exponential Moving Average
--

foreign import ccall unsafe "ta_func.h TA_EMA"
  c_ta_ema :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_ema :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_ema inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_ema 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- EXP                  Vector Arithmetic Exp
--

foreign import ccall unsafe "ta_func.h TA_EXP"
  c_ta_exp :: CInt -> CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_exp :: V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_exp inReal = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_exp 0 (fromIntegral $ len - 1) c_inReal cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- FLOOR                Vector Floor
--

foreign import ccall unsafe "ta_func.h TA_FLOOR"
  c_ta_floor :: CInt -> CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_floor :: V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_floor inReal = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_floor 0 (fromIntegral $ len - 1) c_inReal cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- HT_DCPERIOD          Hilbert Transform - Dominant Cycle Period
--

foreign import ccall unsafe "ta_func.h TA_HT_DCPERIOD"
  c_ta_ht_dcperiod :: CInt -> CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_ht_dcperiod :: V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_ht_dcperiod inReal = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_ht_dcperiod 0 (fromIntegral $ len - 1) c_inReal cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- HT_DCPHASE           Hilbert Transform - Dominant Cycle Phase
--

foreign import ccall unsafe "ta_func.h TA_HT_DCPHASE"
  c_ta_ht_dcphase :: CInt -> CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_ht_dcphase :: V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_ht_dcphase inReal = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_ht_dcphase 0 (fromIntegral $ len - 1) c_inReal cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- HT_PHASOR            Hilbert Transform - Phasor Components
--

foreign import ccall unsafe "ta_func.h TA_HT_PHASOR"
  c_ta_ht_phasor :: CInt -> CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outInPhase (double[])
--   outQuadrature (double[])

ta_ht_phasor :: V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double, V.Vector Double))
ta_ht_phasor inReal = do
    _inReal <- V.unsafeThaw inReal
    _outInPhase <- VM.new len
    _outQuadrature <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outInPhase) $ \c_outInPhase ->
            withForeignPtr (vecPtr _outQuadrature) $ \c_outQuadrature ->
              do rc <- c_ta_ht_phasor 0 (fromIntegral $ len - 1) c_inReal cOutBegIdx cOutNbElement c_outInPhase c_outQuadrature
                 out_outInPhase <- V.unsafeFreeze _outInPhase
                 out_outQuadrature <- V.unsafeFreeze _outQuadrature
                 case rc of
                   0 -> do outBegIdx <- peek cOutBegIdx
                           outNbElement <- peek cOutNbElement
                           return $ Right $ (fromIntegral outBegIdx,
                                             fromIntegral outNbElement,
                                             out_outInPhase,
                                             out_outQuadrature)
                   _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- HT_SINE              Hilbert Transform - SineWave
--

foreign import ccall unsafe "ta_func.h TA_HT_SINE"
  c_ta_ht_sine :: CInt -> CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outSine (double[])
--   outLeadSine (double[])

ta_ht_sine :: V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double, V.Vector Double))
ta_ht_sine inReal = do
    _inReal <- V.unsafeThaw inReal
    _outSine <- VM.new len
    _outLeadSine <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outSine) $ \c_outSine ->
            withForeignPtr (vecPtr _outLeadSine) $ \c_outLeadSine ->
              do rc <- c_ta_ht_sine 0 (fromIntegral $ len - 1) c_inReal cOutBegIdx cOutNbElement c_outSine c_outLeadSine
                 out_outSine <- V.unsafeFreeze _outSine
                 out_outLeadSine <- V.unsafeFreeze _outLeadSine
                 case rc of
                   0 -> do outBegIdx <- peek cOutBegIdx
                           outNbElement <- peek cOutNbElement
                           return $ Right $ (fromIntegral outBegIdx,
                                             fromIntegral outNbElement,
                                             out_outSine,
                                             out_outLeadSine)
                   _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- HT_TRENDLINE         Hilbert Transform - Instantaneous Trendline
--

foreign import ccall unsafe "ta_func.h TA_HT_TRENDLINE"
  c_ta_ht_trendline :: CInt -> CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_ht_trendline :: V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_ht_trendline inReal = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_ht_trendline 0 (fromIntegral $ len - 1) c_inReal cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- HT_TRENDMODE         Hilbert Transform - Trend vs Cycle Mode
--

foreign import ccall unsafe "ta_func.h TA_HT_TRENDMODE"
  c_ta_ht_trendmode :: CInt -> CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outInteger (int[])

ta_ht_trendmode :: V.Vector Double -> IO (Either Int (Int, Int, V.Vector Int))
ta_ht_trendmode inReal = do
    _inReal <- V.unsafeThaw inReal
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
            do rc <- c_ta_ht_trendmode 0 (fromIntegral $ len - 1) c_inReal cOutBegIdx cOutNbElement c_outInteger
               out_outInteger <- V.unsafeFreeze _outInteger
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outInteger)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- KAMA                 Kaufman Adaptive Moving Average
--

foreign import ccall unsafe "ta_func.h TA_KAMA"
  c_ta_kama :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_kama :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_kama inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_kama 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- LINEARREG            Linear Regression
--

foreign import ccall unsafe "ta_func.h TA_LINEARREG"
  c_ta_linearreg :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_linearreg :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_linearreg inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_linearreg 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- LINEARREG_ANGLE      Linear Regression Angle
--

foreign import ccall unsafe "ta_func.h TA_LINEARREG_ANGLE"
  c_ta_linearreg_angle :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_linearreg_angle :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_linearreg_angle inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_linearreg_angle 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- LINEARREG_INTERCEPT  Linear Regression Intercept
--

foreign import ccall unsafe "ta_func.h TA_LINEARREG_INTERCEPT"
  c_ta_linearreg_intercept :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_linearreg_intercept :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_linearreg_intercept inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_linearreg_intercept 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- LINEARREG_SLOPE      Linear Regression Slope
--

foreign import ccall unsafe "ta_func.h TA_LINEARREG_SLOPE"
  c_ta_linearreg_slope :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_linearreg_slope :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_linearreg_slope inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_linearreg_slope 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- LN                   Vector Log Natural
--

foreign import ccall unsafe "ta_func.h TA_LN"
  c_ta_ln :: CInt -> CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_ln :: V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_ln inReal = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_ln 0 (fromIntegral $ len - 1) c_inReal cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- LOG10                Vector Log10
--

foreign import ccall unsafe "ta_func.h TA_LOG10"
  c_ta_log10 :: CInt -> CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_log10 :: V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_log10 inReal = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_log10 0 (fromIntegral $ len - 1) c_inReal cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- MA                   Moving average
--

foreign import ccall unsafe "ta_func.h TA_MA"
  c_ta_ma :: CInt -> CInt -> Ptr Double -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
--   optInMAType (int)
-- outputs
--   outReal (double[])

ta_ma :: V.Vector Double -> Int -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_ma inReal optInTimePeriod optInMAType = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_ma 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) (fromIntegral optInMAType) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- MACD                 Moving Average Convergence/Divergence
--

foreign import ccall unsafe "ta_func.h TA_MACD"
  c_ta_macd :: CInt -> CInt -> Ptr Double -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> Ptr Double -> Ptr Double -> IO CInt

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

ta_macd :: V.Vector Double -> Int -> Int -> Int -> IO (Either Int (Int, Int, V.Vector Double, V.Vector Double, V.Vector Double))
ta_macd inReal optInFastPeriod optInSlowPeriod optInSignalPeriod = do
    _inReal <- V.unsafeThaw inReal
    _outMACD <- VM.new len
    _outMACDSignal <- VM.new len
    _outMACDHist <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outMACD) $ \c_outMACD ->
            withForeignPtr (vecPtr _outMACDSignal) $ \c_outMACDSignal ->
              withForeignPtr (vecPtr _outMACDHist) $ \c_outMACDHist ->
                do rc <- c_ta_macd 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInFastPeriod) (fromIntegral optInSlowPeriod) (fromIntegral optInSignalPeriod) cOutBegIdx cOutNbElement c_outMACD c_outMACDSignal c_outMACDHist
                   out_outMACD <- V.unsafeFreeze _outMACD
                   out_outMACDSignal <- V.unsafeFreeze _outMACDSignal
                   out_outMACDHist <- V.unsafeFreeze _outMACDHist
                   case rc of
                     0 -> do outBegIdx <- peek cOutBegIdx
                             outNbElement <- peek cOutNbElement
                             return $ Right $ (fromIntegral outBegIdx,
                                               fromIntegral outNbElement,
                                               out_outMACD,
                                               out_outMACDSignal,
                                               out_outMACDHist)
                     _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- MACDEXT              MACD with controllable MA type
--

foreign import ccall unsafe "ta_func.h TA_MACDEXT"
  c_ta_macdext :: CInt -> CInt -> Ptr Double -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> Ptr Double -> Ptr Double -> IO CInt

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

ta_macdext :: V.Vector Double -> Int -> Int -> Int -> Int -> Int -> Int -> IO (Either Int (Int, Int, V.Vector Double, V.Vector Double, V.Vector Double))
ta_macdext inReal optInFastPeriod optInFastMAType optInSlowPeriod optInSlowMAType optInSignalPeriod optInSignalMAType = do
    _inReal <- V.unsafeThaw inReal
    _outMACD <- VM.new len
    _outMACDSignal <- VM.new len
    _outMACDHist <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outMACD) $ \c_outMACD ->
            withForeignPtr (vecPtr _outMACDSignal) $ \c_outMACDSignal ->
              withForeignPtr (vecPtr _outMACDHist) $ \c_outMACDHist ->
                do rc <- c_ta_macdext 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInFastPeriod) (fromIntegral optInFastMAType) (fromIntegral optInSlowPeriod) (fromIntegral optInSlowMAType) (fromIntegral optInSignalPeriod) (fromIntegral optInSignalMAType) cOutBegIdx cOutNbElement c_outMACD c_outMACDSignal c_outMACDHist
                   out_outMACD <- V.unsafeFreeze _outMACD
                   out_outMACDSignal <- V.unsafeFreeze _outMACDSignal
                   out_outMACDHist <- V.unsafeFreeze _outMACDHist
                   case rc of
                     0 -> do outBegIdx <- peek cOutBegIdx
                             outNbElement <- peek cOutNbElement
                             return $ Right $ (fromIntegral outBegIdx,
                                               fromIntegral outNbElement,
                                               out_outMACD,
                                               out_outMACDSignal,
                                               out_outMACDHist)
                     _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- MACDFIX              Moving Average Convergence/Divergence Fix 12/26
--

foreign import ccall unsafe "ta_func.h TA_MACDFIX"
  c_ta_macdfix :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> Ptr Double -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInSignalPeriod (int)
-- outputs
--   outMACD (double[])
--   outMACDSignal (double[])
--   outMACDHist (double[])

ta_macdfix :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double, V.Vector Double, V.Vector Double))
ta_macdfix inReal optInSignalPeriod = do
    _inReal <- V.unsafeThaw inReal
    _outMACD <- VM.new len
    _outMACDSignal <- VM.new len
    _outMACDHist <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outMACD) $ \c_outMACD ->
            withForeignPtr (vecPtr _outMACDSignal) $ \c_outMACDSignal ->
              withForeignPtr (vecPtr _outMACDHist) $ \c_outMACDHist ->
                do rc <- c_ta_macdfix 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInSignalPeriod) cOutBegIdx cOutNbElement c_outMACD c_outMACDSignal c_outMACDHist
                   out_outMACD <- V.unsafeFreeze _outMACD
                   out_outMACDSignal <- V.unsafeFreeze _outMACDSignal
                   out_outMACDHist <- V.unsafeFreeze _outMACDHist
                   case rc of
                     0 -> do outBegIdx <- peek cOutBegIdx
                             outNbElement <- peek cOutNbElement
                             return $ Right $ (fromIntegral outBegIdx,
                                               fromIntegral outNbElement,
                                               out_outMACD,
                                               out_outMACDSignal,
                                               out_outMACDHist)
                     _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- MAMA                 MESA Adaptive Moving Average
--

foreign import ccall unsafe "ta_func.h TA_MAMA"
  c_ta_mama :: CInt -> CInt -> Ptr Double -> CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr Double -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInFastLimit (double)
--   optInSlowLimit (double)
-- outputs
--   outMAMA (double[])
--   outFAMA (double[])

ta_mama :: V.Vector Double -> Double -> Double -> IO (Either Int (Int, Int, V.Vector Double, V.Vector Double))
ta_mama inReal optInFastLimit optInSlowLimit = do
    _inReal <- V.unsafeThaw inReal
    _outMAMA <- VM.new len
    _outFAMA <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outMAMA) $ \c_outMAMA ->
            withForeignPtr (vecPtr _outFAMA) $ \c_outFAMA ->
              do rc <- c_ta_mama 0 (fromIntegral $ len - 1) c_inReal (realToFrac optInFastLimit) (realToFrac optInSlowLimit) cOutBegIdx cOutNbElement c_outMAMA c_outFAMA
                 out_outMAMA <- V.unsafeFreeze _outMAMA
                 out_outFAMA <- V.unsafeFreeze _outFAMA
                 case rc of
                   0 -> do outBegIdx <- peek cOutBegIdx
                           outNbElement <- peek cOutNbElement
                           return $ Right $ (fromIntegral outBegIdx,
                                             fromIntegral outNbElement,
                                             out_outMAMA,
                                             out_outFAMA)
                   _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- MAVP                 Moving average with variable period
--

foreign import ccall unsafe "ta_func.h TA_MAVP"
  c_ta_mavp :: CInt -> CInt -> Ptr Double -> Ptr Double -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
--   inPeriods
-- arguments
--   optInMinPeriod (int)
--   optInMaxPeriod (int)
--   optInMAType (int)
-- outputs
--   outReal (double[])

ta_mavp :: V.Vector Double -> V.Vector Double -> Int -> Int -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_mavp inReal inPeriods optInMinPeriod optInMaxPeriod optInMAType = do
    _inReal <- V.unsafeThaw inReal
    _inPeriods <- V.unsafeThaw inPeriods
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      withForeignPtr (vecPtr _inPeriods) $ \c_inPeriods ->
        alloca $ \cOutBegIdx ->
          alloca $ \cOutNbElement ->
            withForeignPtr (vecPtr _outReal) $ \c_outReal ->
              do rc <- c_ta_mavp 0 (fromIntegral $ len - 1) c_inReal c_inPeriods (fromIntegral optInMinPeriod) (fromIntegral optInMaxPeriod) (fromIntegral optInMAType) cOutBegIdx cOutNbElement c_outReal
                 out_outReal <- V.unsafeFreeze _outReal
                 case rc of
                   0 -> do outBegIdx <- peek cOutBegIdx
                           outNbElement <- peek cOutNbElement
                           return $ Right $ (fromIntegral outBegIdx,
                                             fromIntegral outNbElement,
                                             out_outReal)
                   _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- MAX                  Highest value over a specified period
--

foreign import ccall unsafe "ta_func.h TA_MAX"
  c_ta_max :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_max :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_max inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_max 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- MAXINDEX             Index of highest value over a specified period
--

foreign import ccall unsafe "ta_func.h TA_MAXINDEX"
  c_ta_maxindex :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outInteger (int[])

ta_maxindex :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Int))
ta_maxindex inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
            do rc <- c_ta_maxindex 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outInteger
               out_outInteger <- V.unsafeFreeze _outInteger
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outInteger)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- MEDPRICE             Median Price
--

foreign import ccall unsafe "ta_func.h TA_MEDPRICE"
  c_ta_medprice :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inHigh
--   inLow
-- arguments
-- outputs
--   outReal (double[])

ta_medprice :: V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_medprice inHigh inLow = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        alloca $ \cOutBegIdx ->
          alloca $ \cOutNbElement ->
            withForeignPtr (vecPtr _outReal) $ \c_outReal ->
              do rc <- c_ta_medprice 0 (fromIntegral $ len - 1) c_inHigh c_inLow cOutBegIdx cOutNbElement c_outReal
                 out_outReal <- V.unsafeFreeze _outReal
                 case rc of
                   0 -> do outBegIdx <- peek cOutBegIdx
                           outNbElement <- peek cOutNbElement
                           return $ Right $ (fromIntegral outBegIdx,
                                             fromIntegral outNbElement,
                                             out_outReal)
                   _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- MFI                  Money Flow Index
--

foreign import ccall unsafe "ta_func.h TA_MFI"
  c_ta_mfi :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
--   inVolume
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_mfi :: V.Vector Double -> V.Vector Double -> V.Vector Double -> V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_mfi inHigh inLow inClose inVolume optInTimePeriod = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _inVolume <- V.unsafeThaw inVolume
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        withForeignPtr (vecPtr _inClose) $ \c_inClose ->
          withForeignPtr (vecPtr _inVolume) $ \c_inVolume ->
            alloca $ \cOutBegIdx ->
              alloca $ \cOutNbElement ->
                withForeignPtr (vecPtr _outReal) $ \c_outReal ->
                  do rc <- c_ta_mfi 0 (fromIntegral $ len - 1) c_inHigh c_inLow c_inClose c_inVolume (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
                     out_outReal <- V.unsafeFreeze _outReal
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outReal)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- MIDPOINT             MidPoint over period
--

foreign import ccall unsafe "ta_func.h TA_MIDPOINT"
  c_ta_midpoint :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_midpoint :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_midpoint inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_midpoint 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- MIDPRICE             Midpoint Price over period
--

foreign import ccall unsafe "ta_func.h TA_MIDPRICE"
  c_ta_midprice :: CInt -> CInt -> Ptr Double -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inHigh
--   inLow
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_midprice :: V.Vector Double -> V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_midprice inHigh inLow optInTimePeriod = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        alloca $ \cOutBegIdx ->
          alloca $ \cOutNbElement ->
            withForeignPtr (vecPtr _outReal) $ \c_outReal ->
              do rc <- c_ta_midprice 0 (fromIntegral $ len - 1) c_inHigh c_inLow (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
                 out_outReal <- V.unsafeFreeze _outReal
                 case rc of
                   0 -> do outBegIdx <- peek cOutBegIdx
                           outNbElement <- peek cOutNbElement
                           return $ Right $ (fromIntegral outBegIdx,
                                             fromIntegral outNbElement,
                                             out_outReal)
                   _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- MIN                  Lowest value over a specified period
--

foreign import ccall unsafe "ta_func.h TA_MIN"
  c_ta_min :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_min :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_min inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_min 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- MININDEX             Index of lowest value over a specified period
--

foreign import ccall unsafe "ta_func.h TA_MININDEX"
  c_ta_minindex :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Int -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outInteger (int[])

ta_minindex :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Int))
ta_minindex inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outInteger <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outInteger) $ \c_outInteger ->
            do rc <- c_ta_minindex 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outInteger
               out_outInteger <- V.unsafeFreeze _outInteger
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outInteger)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- MINMAX               Lowest and highest values over a specified period
--

foreign import ccall unsafe "ta_func.h TA_MINMAX"
  c_ta_minmax :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outMin (double[])
--   outMax (double[])

ta_minmax :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double, V.Vector Double))
ta_minmax inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outMin <- VM.new len
    _outMax <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outMin) $ \c_outMin ->
            withForeignPtr (vecPtr _outMax) $ \c_outMax ->
              do rc <- c_ta_minmax 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outMin c_outMax
                 out_outMin <- V.unsafeFreeze _outMin
                 out_outMax <- V.unsafeFreeze _outMax
                 case rc of
                   0 -> do outBegIdx <- peek cOutBegIdx
                           outNbElement <- peek cOutNbElement
                           return $ Right $ (fromIntegral outBegIdx,
                                             fromIntegral outNbElement,
                                             out_outMin,
                                             out_outMax)
                   _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- MINMAXINDEX          Indexes of lowest and highest values over a specified period
--

foreign import ccall unsafe "ta_func.h TA_MINMAXINDEX"
  c_ta_minmaxindex :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Int -> Ptr Int -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outMinIdx (int[])
--   outMaxIdx (int[])

ta_minmaxindex :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Int, V.Vector Int))
ta_minmaxindex inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outMinIdx <- VM.new len
    _outMaxIdx <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outMinIdx) $ \c_outMinIdx ->
            withForeignPtr (vecPtr _outMaxIdx) $ \c_outMaxIdx ->
              do rc <- c_ta_minmaxindex 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outMinIdx c_outMaxIdx
                 out_outMinIdx <- V.unsafeFreeze _outMinIdx
                 out_outMaxIdx <- V.unsafeFreeze _outMaxIdx
                 case rc of
                   0 -> do outBegIdx <- peek cOutBegIdx
                           outNbElement <- peek cOutNbElement
                           return $ Right $ (fromIntegral outBegIdx,
                                             fromIntegral outNbElement,
                                             out_outMinIdx,
                                             out_outMaxIdx)
                   _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- MINUS_DI             Minus Directional Indicator
--

foreign import ccall unsafe "ta_func.h TA_MINUS_DI"
  c_ta_minus_di :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_minus_di :: V.Vector Double -> V.Vector Double -> V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_minus_di inHigh inLow inClose optInTimePeriod = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        withForeignPtr (vecPtr _inClose) $ \c_inClose ->
          alloca $ \cOutBegIdx ->
            alloca $ \cOutNbElement ->
              withForeignPtr (vecPtr _outReal) $ \c_outReal ->
                do rc <- c_ta_minus_di 0 (fromIntegral $ len - 1) c_inHigh c_inLow c_inClose (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
                   out_outReal <- V.unsafeFreeze _outReal
                   case rc of
                     0 -> do outBegIdx <- peek cOutBegIdx
                             outNbElement <- peek cOutNbElement
                             return $ Right $ (fromIntegral outBegIdx,
                                               fromIntegral outNbElement,
                                               out_outReal)
                     _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- MINUS_DM             Minus Directional Movement
--

foreign import ccall unsafe "ta_func.h TA_MINUS_DM"
  c_ta_minus_dm :: CInt -> CInt -> Ptr Double -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inHigh
--   inLow
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_minus_dm :: V.Vector Double -> V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_minus_dm inHigh inLow optInTimePeriod = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        alloca $ \cOutBegIdx ->
          alloca $ \cOutNbElement ->
            withForeignPtr (vecPtr _outReal) $ \c_outReal ->
              do rc <- c_ta_minus_dm 0 (fromIntegral $ len - 1) c_inHigh c_inLow (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
                 out_outReal <- V.unsafeFreeze _outReal
                 case rc of
                   0 -> do outBegIdx <- peek cOutBegIdx
                           outNbElement <- peek cOutNbElement
                           return $ Right $ (fromIntegral outBegIdx,
                                             fromIntegral outNbElement,
                                             out_outReal)
                   _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- MOM                  Momentum
--

foreign import ccall unsafe "ta_func.h TA_MOM"
  c_ta_mom :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_mom :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_mom inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_mom 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- MULT                 Vector Arithmetic Mult
--

foreign import ccall unsafe "ta_func.h TA_MULT"
  c_ta_mult :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal0
--   inReal1
-- arguments
-- outputs
--   outReal (double[])

ta_mult :: V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_mult inReal0 inReal1 = do
    _inReal0 <- V.unsafeThaw inReal0
    _inReal1 <- V.unsafeThaw inReal1
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal0) $ \c_inReal0 ->
      withForeignPtr (vecPtr _inReal1) $ \c_inReal1 ->
        alloca $ \cOutBegIdx ->
          alloca $ \cOutNbElement ->
            withForeignPtr (vecPtr _outReal) $ \c_outReal ->
              do rc <- c_ta_mult 0 (fromIntegral $ len - 1) c_inReal0 c_inReal1 cOutBegIdx cOutNbElement c_outReal
                 out_outReal <- V.unsafeFreeze _outReal
                 case rc of
                   0 -> do outBegIdx <- peek cOutBegIdx
                           outNbElement <- peek cOutNbElement
                           return $ Right $ (fromIntegral outBegIdx,
                                             fromIntegral outNbElement,
                                             out_outReal)
                   _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal0

--
-- NATR                 Normalized Average True Range
--

foreign import ccall unsafe "ta_func.h TA_NATR"
  c_ta_natr :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_natr :: V.Vector Double -> V.Vector Double -> V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_natr inHigh inLow inClose optInTimePeriod = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        withForeignPtr (vecPtr _inClose) $ \c_inClose ->
          alloca $ \cOutBegIdx ->
            alloca $ \cOutNbElement ->
              withForeignPtr (vecPtr _outReal) $ \c_outReal ->
                do rc <- c_ta_natr 0 (fromIntegral $ len - 1) c_inHigh c_inLow c_inClose (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
                   out_outReal <- V.unsafeFreeze _outReal
                   case rc of
                     0 -> do outBegIdx <- peek cOutBegIdx
                             outNbElement <- peek cOutNbElement
                             return $ Right $ (fromIntegral outBegIdx,
                                               fromIntegral outNbElement,
                                               out_outReal)
                     _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- OBV                  On Balance Volume
--

foreign import ccall unsafe "ta_func.h TA_OBV"
  c_ta_obv :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
--   inVolume1
-- arguments
-- outputs
--   outReal (double[])

ta_obv :: V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_obv inReal inVolume1 = do
    _inReal <- V.unsafeThaw inReal
    _inVolume1 <- V.unsafeThaw inVolume1
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      withForeignPtr (vecPtr _inVolume1) $ \c_inVolume1 ->
        alloca $ \cOutBegIdx ->
          alloca $ \cOutNbElement ->
            withForeignPtr (vecPtr _outReal) $ \c_outReal ->
              do rc <- c_ta_obv 0 (fromIntegral $ len - 1) c_inReal c_inVolume1 cOutBegIdx cOutNbElement c_outReal
                 out_outReal <- V.unsafeFreeze _outReal
                 case rc of
                   0 -> do outBegIdx <- peek cOutBegIdx
                           outNbElement <- peek cOutNbElement
                           return $ Right $ (fromIntegral outBegIdx,
                                             fromIntegral outNbElement,
                                             out_outReal)
                   _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- PLUS_DI              Plus Directional Indicator
--

foreign import ccall unsafe "ta_func.h TA_PLUS_DI"
  c_ta_plus_di :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_plus_di :: V.Vector Double -> V.Vector Double -> V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_plus_di inHigh inLow inClose optInTimePeriod = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        withForeignPtr (vecPtr _inClose) $ \c_inClose ->
          alloca $ \cOutBegIdx ->
            alloca $ \cOutNbElement ->
              withForeignPtr (vecPtr _outReal) $ \c_outReal ->
                do rc <- c_ta_plus_di 0 (fromIntegral $ len - 1) c_inHigh c_inLow c_inClose (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
                   out_outReal <- V.unsafeFreeze _outReal
                   case rc of
                     0 -> do outBegIdx <- peek cOutBegIdx
                             outNbElement <- peek cOutNbElement
                             return $ Right $ (fromIntegral outBegIdx,
                                               fromIntegral outNbElement,
                                               out_outReal)
                     _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- PLUS_DM              Plus Directional Movement
--

foreign import ccall unsafe "ta_func.h TA_PLUS_DM"
  c_ta_plus_dm :: CInt -> CInt -> Ptr Double -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inHigh
--   inLow
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_plus_dm :: V.Vector Double -> V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_plus_dm inHigh inLow optInTimePeriod = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        alloca $ \cOutBegIdx ->
          alloca $ \cOutNbElement ->
            withForeignPtr (vecPtr _outReal) $ \c_outReal ->
              do rc <- c_ta_plus_dm 0 (fromIntegral $ len - 1) c_inHigh c_inLow (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
                 out_outReal <- V.unsafeFreeze _outReal
                 case rc of
                   0 -> do outBegIdx <- peek cOutBegIdx
                           outNbElement <- peek cOutNbElement
                           return $ Right $ (fromIntegral outBegIdx,
                                             fromIntegral outNbElement,
                                             out_outReal)
                   _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- PPO                  Percentage Price Oscillator
--

foreign import ccall unsafe "ta_func.h TA_PPO"
  c_ta_ppo :: CInt -> CInt -> Ptr Double -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInFastPeriod (int)
--   optInSlowPeriod (int)
--   optInMAType (int)
-- outputs
--   outReal (double[])

ta_ppo :: V.Vector Double -> Int -> Int -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_ppo inReal optInFastPeriod optInSlowPeriod optInMAType = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_ppo 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInFastPeriod) (fromIntegral optInSlowPeriod) (fromIntegral optInMAType) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- ROC                  Rate of change : ((price/prevPrice)-1)*100
--

foreign import ccall unsafe "ta_func.h TA_ROC"
  c_ta_roc :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_roc :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_roc inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_roc 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- ROCP                 Rate of change Percentage: (price-prevPrice)/prevPrice
--

foreign import ccall unsafe "ta_func.h TA_ROCP"
  c_ta_rocp :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_rocp :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_rocp inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_rocp 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- ROCR                 Rate of change ratio: (price/prevPrice)
--

foreign import ccall unsafe "ta_func.h TA_ROCR"
  c_ta_rocr :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_rocr :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_rocr inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_rocr 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- ROCR100              Rate of change ratio 100 scale: (price/prevPrice)*100
--

foreign import ccall unsafe "ta_func.h TA_ROCR100"
  c_ta_rocr100 :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_rocr100 :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_rocr100 inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_rocr100 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- RSI                  Relative Strength Index
--

foreign import ccall unsafe "ta_func.h TA_RSI"
  c_ta_rsi :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_rsi :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_rsi inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_rsi 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- SAR                  Parabolic SAR
--

foreign import ccall unsafe "ta_func.h TA_SAR"
  c_ta_sar :: CInt -> CInt -> Ptr Double -> Ptr Double -> CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inHigh
--   inLow
-- arguments
--   optInAcceleration (double)
--   optInMaximum (double)
-- outputs
--   outReal (double[])

ta_sar :: V.Vector Double -> V.Vector Double -> Double -> Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_sar inHigh inLow optInAcceleration optInMaximum = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        alloca $ \cOutBegIdx ->
          alloca $ \cOutNbElement ->
            withForeignPtr (vecPtr _outReal) $ \c_outReal ->
              do rc <- c_ta_sar 0 (fromIntegral $ len - 1) c_inHigh c_inLow (realToFrac optInAcceleration) (realToFrac optInMaximum) cOutBegIdx cOutNbElement c_outReal
                 out_outReal <- V.unsafeFreeze _outReal
                 case rc of
                   0 -> do outBegIdx <- peek cOutBegIdx
                           outNbElement <- peek cOutNbElement
                           return $ Right $ (fromIntegral outBegIdx,
                                             fromIntegral outNbElement,
                                             out_outReal)
                   _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- SAREXT               Parabolic SAR - Extended
--

foreign import ccall unsafe "ta_func.h TA_SAREXT"
  c_ta_sarext :: CInt -> CInt -> Ptr Double -> Ptr Double -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

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

ta_sarext :: V.Vector Double -> V.Vector Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_sarext inHigh inLow optInStartValue optInOffsetOnReverse optInAccelerationInitLong optInAccelerationLong optInAccelerationMaxLong optInAccelerationInitShort optInAccelerationShort optInAccelerationMaxShort = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        alloca $ \cOutBegIdx ->
          alloca $ \cOutNbElement ->
            withForeignPtr (vecPtr _outReal) $ \c_outReal ->
              do rc <- c_ta_sarext 0 (fromIntegral $ len - 1) c_inHigh c_inLow (realToFrac optInStartValue) (realToFrac optInOffsetOnReverse) (realToFrac optInAccelerationInitLong) (realToFrac optInAccelerationLong) (realToFrac optInAccelerationMaxLong) (realToFrac optInAccelerationInitShort) (realToFrac optInAccelerationShort) (realToFrac optInAccelerationMaxShort) cOutBegIdx cOutNbElement c_outReal
                 out_outReal <- V.unsafeFreeze _outReal
                 case rc of
                   0 -> do outBegIdx <- peek cOutBegIdx
                           outNbElement <- peek cOutNbElement
                           return $ Right $ (fromIntegral outBegIdx,
                                             fromIntegral outNbElement,
                                             out_outReal)
                   _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- SIN                  Vector Trigonometric Sin
--

foreign import ccall unsafe "ta_func.h TA_SIN"
  c_ta_sin :: CInt -> CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_sin :: V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_sin inReal = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_sin 0 (fromIntegral $ len - 1) c_inReal cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- SINH                 Vector Trigonometric Sinh
--

foreign import ccall unsafe "ta_func.h TA_SINH"
  c_ta_sinh :: CInt -> CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_sinh :: V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_sinh inReal = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_sinh 0 (fromIntegral $ len - 1) c_inReal cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- SMA                  Simple Moving Average
--

foreign import ccall unsafe "ta_func.h TA_SMA"
  c_ta_sma :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_sma :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_sma inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_sma 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- SQRT                 Vector Square Root
--

foreign import ccall unsafe "ta_func.h TA_SQRT"
  c_ta_sqrt :: CInt -> CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_sqrt :: V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_sqrt inReal = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_sqrt 0 (fromIntegral $ len - 1) c_inReal cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- STDDEV               Standard Deviation
--

foreign import ccall unsafe "ta_func.h TA_STDDEV"
  c_ta_stddev :: CInt -> CInt -> Ptr Double -> CInt -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
--   optInNbDev (double)
-- outputs
--   outReal (double[])

ta_stddev :: V.Vector Double -> Int -> Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_stddev inReal optInTimePeriod optInNbDev = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_stddev 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) (realToFrac optInNbDev) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- STOCH                Stochastic
--

foreign import ccall unsafe "ta_func.h TA_STOCH"
  c_ta_stoch :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> Ptr Double -> IO CInt

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

ta_stoch :: V.Vector Double -> V.Vector Double -> V.Vector Double -> Int -> Int -> Int -> Int -> Int -> IO (Either Int (Int, Int, V.Vector Double, V.Vector Double))
ta_stoch inHigh inLow inClose optInFastK_Period optInSlowK_Period optInSlowK_MAType optInSlowD_Period optInSlowD_MAType = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outSlowK <- VM.new len
    _outSlowD <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        withForeignPtr (vecPtr _inClose) $ \c_inClose ->
          alloca $ \cOutBegIdx ->
            alloca $ \cOutNbElement ->
              withForeignPtr (vecPtr _outSlowK) $ \c_outSlowK ->
                withForeignPtr (vecPtr _outSlowD) $ \c_outSlowD ->
                  do rc <- c_ta_stoch 0 (fromIntegral $ len - 1) c_inHigh c_inLow c_inClose (fromIntegral optInFastK_Period) (fromIntegral optInSlowK_Period) (fromIntegral optInSlowK_MAType) (fromIntegral optInSlowD_Period) (fromIntegral optInSlowD_MAType) cOutBegIdx cOutNbElement c_outSlowK c_outSlowD
                     out_outSlowK <- V.unsafeFreeze _outSlowK
                     out_outSlowD <- V.unsafeFreeze _outSlowD
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outSlowK,
                                                 out_outSlowD)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- STOCHF               Stochastic Fast
--

foreign import ccall unsafe "ta_func.h TA_STOCHF"
  c_ta_stochf :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> Ptr Double -> IO CInt

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

ta_stochf :: V.Vector Double -> V.Vector Double -> V.Vector Double -> Int -> Int -> Int -> IO (Either Int (Int, Int, V.Vector Double, V.Vector Double))
ta_stochf inHigh inLow inClose optInFastK_Period optInFastD_Period optInFastD_MAType = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outFastK <- VM.new len
    _outFastD <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        withForeignPtr (vecPtr _inClose) $ \c_inClose ->
          alloca $ \cOutBegIdx ->
            alloca $ \cOutNbElement ->
              withForeignPtr (vecPtr _outFastK) $ \c_outFastK ->
                withForeignPtr (vecPtr _outFastD) $ \c_outFastD ->
                  do rc <- c_ta_stochf 0 (fromIntegral $ len - 1) c_inHigh c_inLow c_inClose (fromIntegral optInFastK_Period) (fromIntegral optInFastD_Period) (fromIntegral optInFastD_MAType) cOutBegIdx cOutNbElement c_outFastK c_outFastD
                     out_outFastK <- V.unsafeFreeze _outFastK
                     out_outFastD <- V.unsafeFreeze _outFastD
                     case rc of
                       0 -> do outBegIdx <- peek cOutBegIdx
                               outNbElement <- peek cOutNbElement
                               return $ Right $ (fromIntegral outBegIdx,
                                                 fromIntegral outNbElement,
                                                 out_outFastK,
                                                 out_outFastD)
                       _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- STOCHRSI             Stochastic Relative Strength Index
--

foreign import ccall unsafe "ta_func.h TA_STOCHRSI"
  c_ta_stochrsi :: CInt -> CInt -> Ptr Double -> CInt -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> Ptr Double -> IO CInt

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

ta_stochrsi :: V.Vector Double -> Int -> Int -> Int -> Int -> IO (Either Int (Int, Int, V.Vector Double, V.Vector Double))
ta_stochrsi inReal optInTimePeriod optInFastK_Period optInFastD_Period optInFastD_MAType = do
    _inReal <- V.unsafeThaw inReal
    _outFastK <- VM.new len
    _outFastD <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outFastK) $ \c_outFastK ->
            withForeignPtr (vecPtr _outFastD) $ \c_outFastD ->
              do rc <- c_ta_stochrsi 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) (fromIntegral optInFastK_Period) (fromIntegral optInFastD_Period) (fromIntegral optInFastD_MAType) cOutBegIdx cOutNbElement c_outFastK c_outFastD
                 out_outFastK <- V.unsafeFreeze _outFastK
                 out_outFastD <- V.unsafeFreeze _outFastD
                 case rc of
                   0 -> do outBegIdx <- peek cOutBegIdx
                           outNbElement <- peek cOutNbElement
                           return $ Right $ (fromIntegral outBegIdx,
                                             fromIntegral outNbElement,
                                             out_outFastK,
                                             out_outFastD)
                   _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- SUB                  Vector Arithmetic Substraction
--

foreign import ccall unsafe "ta_func.h TA_SUB"
  c_ta_sub :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal0
--   inReal1
-- arguments
-- outputs
--   outReal (double[])

ta_sub :: V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_sub inReal0 inReal1 = do
    _inReal0 <- V.unsafeThaw inReal0
    _inReal1 <- V.unsafeThaw inReal1
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal0) $ \c_inReal0 ->
      withForeignPtr (vecPtr _inReal1) $ \c_inReal1 ->
        alloca $ \cOutBegIdx ->
          alloca $ \cOutNbElement ->
            withForeignPtr (vecPtr _outReal) $ \c_outReal ->
              do rc <- c_ta_sub 0 (fromIntegral $ len - 1) c_inReal0 c_inReal1 cOutBegIdx cOutNbElement c_outReal
                 out_outReal <- V.unsafeFreeze _outReal
                 case rc of
                   0 -> do outBegIdx <- peek cOutBegIdx
                           outNbElement <- peek cOutNbElement
                           return $ Right $ (fromIntegral outBegIdx,
                                             fromIntegral outNbElement,
                                             out_outReal)
                   _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal0

--
-- SUM                  Summation
--

foreign import ccall unsafe "ta_func.h TA_SUM"
  c_ta_sum :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_sum :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_sum inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_sum 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- T3                   Triple Exponential Moving Average (T3)
--

foreign import ccall unsafe "ta_func.h TA_T3"
  c_ta_t3 :: CInt -> CInt -> Ptr Double -> CInt -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
--   optInVFactor (double)
-- outputs
--   outReal (double[])

ta_t3 :: V.Vector Double -> Int -> Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_t3 inReal optInTimePeriod optInVFactor = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_t3 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) (realToFrac optInVFactor) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- TAN                  Vector Trigonometric Tan
--

foreign import ccall unsafe "ta_func.h TA_TAN"
  c_ta_tan :: CInt -> CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_tan :: V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_tan inReal = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_tan 0 (fromIntegral $ len - 1) c_inReal cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- TANH                 Vector Trigonometric Tanh
--

foreign import ccall unsafe "ta_func.h TA_TANH"
  c_ta_tanh :: CInt -> CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
-- outputs
--   outReal (double[])

ta_tanh :: V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_tanh inReal = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_tanh 0 (fromIntegral $ len - 1) c_inReal cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- TEMA                 Triple Exponential Moving Average
--

foreign import ccall unsafe "ta_func.h TA_TEMA"
  c_ta_tema :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_tema :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_tema inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_tema 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- TRANGE               True Range
--

foreign import ccall unsafe "ta_func.h TA_TRANGE"
  c_ta_trange :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outReal (double[])

ta_trange :: V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_trange inHigh inLow inClose = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        withForeignPtr (vecPtr _inClose) $ \c_inClose ->
          alloca $ \cOutBegIdx ->
            alloca $ \cOutNbElement ->
              withForeignPtr (vecPtr _outReal) $ \c_outReal ->
                do rc <- c_ta_trange 0 (fromIntegral $ len - 1) c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outReal
                   out_outReal <- V.unsafeFreeze _outReal
                   case rc of
                     0 -> do outBegIdx <- peek cOutBegIdx
                             outNbElement <- peek cOutNbElement
                             return $ Right $ (fromIntegral outBegIdx,
                                               fromIntegral outNbElement,
                                               out_outReal)
                     _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- TRIMA                Triangular Moving Average
--

foreign import ccall unsafe "ta_func.h TA_TRIMA"
  c_ta_trima :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_trima :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_trima inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_trima 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- TRIX                 1-day Rate-Of-Change (ROC) of a Triple Smooth EMA
--

foreign import ccall unsafe "ta_func.h TA_TRIX"
  c_ta_trix :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_trix :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_trix inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_trix 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- TSF                  Time Series Forecast
--

foreign import ccall unsafe "ta_func.h TA_TSF"
  c_ta_tsf :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_tsf :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_tsf inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_tsf 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- TYPPRICE             Typical Price
--

foreign import ccall unsafe "ta_func.h TA_TYPPRICE"
  c_ta_typprice :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outReal (double[])

ta_typprice :: V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_typprice inHigh inLow inClose = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        withForeignPtr (vecPtr _inClose) $ \c_inClose ->
          alloca $ \cOutBegIdx ->
            alloca $ \cOutNbElement ->
              withForeignPtr (vecPtr _outReal) $ \c_outReal ->
                do rc <- c_ta_typprice 0 (fromIntegral $ len - 1) c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outReal
                   out_outReal <- V.unsafeFreeze _outReal
                   case rc of
                     0 -> do outBegIdx <- peek cOutBegIdx
                             outNbElement <- peek cOutNbElement
                             return $ Right $ (fromIntegral outBegIdx,
                                               fromIntegral outNbElement,
                                               out_outReal)
                     _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- ULTOSC               Ultimate Oscillator
--

foreign import ccall unsafe "ta_func.h TA_ULTOSC"
  c_ta_ultosc :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

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

ta_ultosc :: V.Vector Double -> V.Vector Double -> V.Vector Double -> Int -> Int -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_ultosc inHigh inLow inClose optInTimePeriod1 optInTimePeriod2 optInTimePeriod3 = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        withForeignPtr (vecPtr _inClose) $ \c_inClose ->
          alloca $ \cOutBegIdx ->
            alloca $ \cOutNbElement ->
              withForeignPtr (vecPtr _outReal) $ \c_outReal ->
                do rc <- c_ta_ultosc 0 (fromIntegral $ len - 1) c_inHigh c_inLow c_inClose (fromIntegral optInTimePeriod1) (fromIntegral optInTimePeriod2) (fromIntegral optInTimePeriod3) cOutBegIdx cOutNbElement c_outReal
                   out_outReal <- V.unsafeFreeze _outReal
                   case rc of
                     0 -> do outBegIdx <- peek cOutBegIdx
                             outNbElement <- peek cOutNbElement
                             return $ Right $ (fromIntegral outBegIdx,
                                               fromIntegral outNbElement,
                                               out_outReal)
                     _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- VAR                  Variance
--

foreign import ccall unsafe "ta_func.h TA_VAR"
  c_ta_var :: CInt -> CInt -> Ptr Double -> CInt -> CDouble -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
--   optInNbDev (double)
-- outputs
--   outReal (double[])

ta_var :: V.Vector Double -> Int -> Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_var inReal optInTimePeriod optInNbDev = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_var 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) (realToFrac optInNbDev) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

--
-- WCLPRICE             Weighted Close Price
--

foreign import ccall unsafe "ta_func.h TA_WCLPRICE"
  c_ta_wclprice :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
-- outputs
--   outReal (double[])

ta_wclprice :: V.Vector Double -> V.Vector Double -> V.Vector Double -> IO (Either Int (Int, Int, V.Vector Double))
ta_wclprice inHigh inLow inClose = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        withForeignPtr (vecPtr _inClose) $ \c_inClose ->
          alloca $ \cOutBegIdx ->
            alloca $ \cOutNbElement ->
              withForeignPtr (vecPtr _outReal) $ \c_outReal ->
                do rc <- c_ta_wclprice 0 (fromIntegral $ len - 1) c_inHigh c_inLow c_inClose cOutBegIdx cOutNbElement c_outReal
                   out_outReal <- V.unsafeFreeze _outReal
                   case rc of
                     0 -> do outBegIdx <- peek cOutBegIdx
                             outNbElement <- peek cOutNbElement
                             return $ Right $ (fromIntegral outBegIdx,
                                               fromIntegral outNbElement,
                                               out_outReal)
                     _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- WILLR                Williams' %R
--

foreign import ccall unsafe "ta_func.h TA_WILLR"
  c_ta_willr :: CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inHigh
--   inLow
--   inClose
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_willr :: V.Vector Double -> V.Vector Double -> V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_willr inHigh inLow inClose optInTimePeriod = do
    _inHigh <- V.unsafeThaw inHigh
    _inLow <- V.unsafeThaw inLow
    _inClose <- V.unsafeThaw inClose
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inHigh) $ \c_inHigh ->
      withForeignPtr (vecPtr _inLow) $ \c_inLow ->
        withForeignPtr (vecPtr _inClose) $ \c_inClose ->
          alloca $ \cOutBegIdx ->
            alloca $ \cOutNbElement ->
              withForeignPtr (vecPtr _outReal) $ \c_outReal ->
                do rc <- c_ta_willr 0 (fromIntegral $ len - 1) c_inHigh c_inLow c_inClose (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
                   out_outReal <- V.unsafeFreeze _outReal
                   case rc of
                     0 -> do outBegIdx <- peek cOutBegIdx
                             outNbElement <- peek cOutNbElement
                             return $ Right $ (fromIntegral outBegIdx,
                                               fromIntegral outNbElement,
                                               out_outReal)
                     _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inHigh

--
-- WMA                  Weighted Moving Average
--

foreign import ccall unsafe "ta_func.h TA_WMA"
  c_ta_wma :: CInt -> CInt -> Ptr Double -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO CInt

-- inputs
--   inReal
-- arguments
--   optInTimePeriod (int)
-- outputs
--   outReal (double[])

ta_wma :: V.Vector Double -> Int -> IO (Either Int (Int, Int, V.Vector Double))
ta_wma inReal optInTimePeriod = do
    _inReal <- V.unsafeThaw inReal
    _outReal <- VM.new len
    withForeignPtr (vecPtr _inReal) $ \c_inReal ->
      alloca $ \cOutBegIdx ->
        alloca $ \cOutNbElement ->
          withForeignPtr (vecPtr _outReal) $ \c_outReal ->
            do rc <- c_ta_wma 0 (fromIntegral $ len - 1) c_inReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement c_outReal
               out_outReal <- V.unsafeFreeze _outReal
               case rc of
                 0 -> do outBegIdx <- peek cOutBegIdx
                         outNbElement <- peek cOutNbElement
                         return $ Right $ (fromIntegral outBegIdx,
                                           fromIntegral outNbElement,
                                           out_outReal)
                 _ -> return $ Left $ fromIntegral rc
  where
    len = fromIntegral $ V.length inReal

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
        
        let inReal = V.fromList(close)
        let inOpen = V.fromList(map (3 +) close)
        let inHigh = V.fromList(map (1 +) close)
        let inLow = V.fromList(map (2 +) close)
        let inClose = V.fromList(map (2 *) close)
        let inVolume = V.fromList(map (fromIntegral . round . (4*)) close)
        
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
        
        putStrLn "High-Wave Candle"
        result <- ta_cdlhighwave inOpen inHigh inLow inClose
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
        
        
        
        
        
        
        
        