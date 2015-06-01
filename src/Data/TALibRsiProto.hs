module Data.TALibRsiProto where

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

vecPtr :: VM.Storable a => VM.MVector s a -> ForeignPtr a
vecPtr = fst . VM.unsafeToForeignPtr0

-- TA-Lib : Technical Analysis Library

foreign import ccall unsafe "ta_common.h TA_Initialize"
  c_ta_init :: IO ()
               
foreign import ccall unsafe "ta_func.h TA_RSI"
  c_ta_rsi :: CInt -> CInt -> Ptr CDouble -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

ta_rsi :: V.Vector CDouble -> Int -> IO (Either Int (Int, Int, V.Vector CDouble))
ta_rsi inReal optInTimePeriod = do
       _inReal <- V.thaw inReal
       v <- VM.new len
       withForeignPtr (vecPtr _inReal)  $ \cInReal ->
         alloca                         $ \cOutBegIdx ->
         alloca                         $ \cOutNbElement ->
         withForeignPtr (vecPtr v)      $ \cOutReal ->
         do rc <- c_ta_rsi startIdx endIdx cInReal (fromIntegral optInTimePeriod) cOutBegIdx cOutNbElement cOutReal
            V.freeze _inReal -- necessary?
            out <- V.freeze v
            case rc of
              0 -> do outBegIdx <- peek cOutBegIdx
                      outNbElement <- peek cOutNbElement
                      return $ Right $ (fromIntegral outBegIdx,
                                        fromIntegral outNbElement,
                                        out
                                       )
              _ -> return $ Left $ fromIntegral rc
    where len = fromIntegral $ V.length inReal
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = 1

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
        
        let vs = V.fromList(inReal)
        
        putStrLn "Relative Strength Index"
        result <- ta_rsi vs 9
        print result
        terpri
        
        