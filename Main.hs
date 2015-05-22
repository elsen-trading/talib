import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.IO.Unsafe
import Data.List.Split
import qualified Data.Vector as V

-- TODO: C functions return an enum, not a CInt

foreign import ccall unsafe "ta_common.h TA_Initialize"
  c_ta_init :: IO ()

-- TA_RetCode TA_MA( int          startIdx,
--                   int          endIdx,
--                   const double inReal[],
--                   int          optInTimePeriod,
--                   int          optInMAType,
--                   int         *outBegIdx,
--                   int         *outNbElement,
--                   double       outReal[] );
foreign import ccall unsafe "ta_func.h TA_MA"
  c_ta_ma :: CInt -> CInt -> Ptr CDouble -> CInt -> CInt ->
             Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- TA_RetCode TA_MEDPRICE( int    startIdx,
--                         int    endIdx,
--                         const double inHigh[],
--                         const double inLow[],
--                         int          *outBegIdx,
--                         int          *outNBElement,
--                         double        outReal[] );
foreign import ccall unsafe "ta_func.h TA_MEDPRICE"
  c_ta_medprice :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> 
                   Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- TA_RetCode TA_ADOSC( int    startIdx,
--                      int    endIdx,
--                      const double inHigh[],
--                      const double inLow[],
--                      const double inClose[],
--                      const double inVolume[],
--                      int           optInFastPeriod, /* From 2 to 100000 */
--                      int           optInSlowPeriod, /* From 2 to 100000 */
--                      int          *outBegIdx,
--                      int          *outNBElement,
--                      double        outReal[] );
foreign import ccall unsafe "ta_func.h TA_ADOSC"
  c_ta_adosc :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble ->
                Ptr CDouble -> Ptr CDouble -> CInt -> CInt ->
                Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

-- TA_RetCode TA_AROON( int    startIdx,
--                      int    endIdx,
--                      const double inHigh[],
--                      const double inLow[],
--                      int           optInTimePeriod, /* From 2 to 100000 */
--                      int          *outBegIdx,
--                      int          *outNBElement,
--                      double        outAroonDown[],
--                      double        outAroonUp[] );
foreign import ccall unsafe "ta_func.h TA_AROON"
  c_ta_aroon :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> CInt ->
                Ptr CInt -> Ptr CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt
                   
-- TaInput is a list of [Double]s, where each [Double] corresponds to e.g., inReal, inHigh, inLow,
-- inClose, etc., depending on the TA function
data TaInput = TaInput [[Double]]

data TaOutput = TaOutput { outBegIdx :: Int
                         , outNBElement :: Int
                         , out :: [[Double]]
                         } deriving (Show)

data TSFun = MovingAverage Int Int -- optInTimePeriod, optInMAType
           | MedianPrice
           | ChaikinAdOscillator Int Int -- optInFastPeriod, optInSlowPeriod
           | Aroon Int -- optInTimePeriod
           deriving (Show)
                        
taIntDefault = fromIntegral (minBound :: CInt)

ta_lib :: TSFun -> TaInput -> IO (Either Int TaOutput)
ta_lib tsfun (TaInput seriess)
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
          MovingAverage timePeriod maType -> c_ta_ma startIdx endIdx cInReal (fromIntegral timePeriod)
                                             (fromIntegral maType) cOutBegIdx cOutNbElement cOutReal
          MedianPrice                     -> c_ta_medprice startIdx endIdx cInReal (getInArrPtr 1)
                                             cOutBegIdx cOutNbElement cOutReal
          ChaikinAdOscillator fast slow   -> c_ta_adosc startIdx endIdx cInReal (getInArrPtr 1)
                                             (getInArrPtr 2) (getInArrPtr 3) (fromIntegral fast)
                                             (fromIntegral slow)
                                             cOutBegIdx cOutNbElement cOutReal
          Aroon timePeriod                -> c_ta_aroon startIdx endIdx cInReal (getInArrPtr 1)
                                             (fromIntegral timePeriod) cOutBegIdx cOutNbElement
                                             cOutReal (getOutArrPtr 1)
        case rc of
          0 -> do
               outReal <- peekArray (len * outputs) cOutReal
               outBegIdx <- peek cOutBegIdx
               outNbElement <- peek cOutNbElement
               return $ Right $ TaOutput { outBegIdx=(fromIntegral outBegIdx),
                                           outNBElement=(fromIntegral outNbElement),
                                           out=chunksOf len (map realToFrac outReal)
                                         }
          _ -> return $ Left $ fromIntegral rc
    where inReal = map realToFrac $ concat seriess
          len = fromIntegral $ length (head seriess)
          startIdx = 0
          endIdx = fromIntegral $ len - 1
          outputs = case tsfun of
            Aroon _ -> 2
            _       -> 1
          
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
        
        let movAvg = MovingAverage 5 taIntDefault
        print movAvg
        result <- ta_lib movAvg (TaInput [close])
        print result
        terpri
        
        let medPrice = MedianPrice
        print medPrice
        result <- ta_lib medPrice (TaInput [open, close])
        print result
        terpri
        
        let aroon = Aroon 5
        print aroon
        result <- ta_lib aroon (TaInput [open, close])
        print result
        terpri
	