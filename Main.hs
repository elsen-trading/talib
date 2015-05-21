import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.IO.Unsafe
import qualified Data.Vector as V

foreign import ccall unsafe "ta_common.h TA_Initialize"
  c_ta_init :: IO ()

-- just to make sure we're calling the C code
foreign import ccall unsafe "ta_func.h TA_ATR_Lookback"
  c_ta_atr_lookback :: CInt -> CInt
                       
-- c interface
-- TA_RetCode TA_MA( int          startIdx,
--                   int          endIdx,
--                   const double inReal[],
--                   int          optInTimePeriod,
--                   int          optInMAType,
--                   int         *outBegIdx,
--                   int         *outNbElement,
--                   double       outReal[],
--                 )

-- TODO: This returns an enum, not an int or ()
foreign import ccall unsafe "ta_func.h TA_MA"
  c_ta_ma :: CInt -> CInt -> Ptr CDouble -> CInt -> CInt ->
             Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

ta_ma :: Int -> [Double] -> IO (Either Int [Double])
ta_ma window series
    = withArray inReal $ \cInReal ->
      alloca           $ \cOutBegIdx ->
      alloca           $ \cOutNbElement ->
      allocaArray len  $ \cOutReal ->
      do
        putStrLn $ "inReal " ++ (show inReal)
        putStrLn $ "endIdx " ++ (show endIdx)
        putStrLn $ "taIntDefault " ++ (show taIntDefault)
        
        rc <- c_ta_ma 0 endIdx cInReal cWindow taIntDefault cOutBegIdx cOutNbElement cOutReal
        case rc of
          0 -> do
               val <-peekArray len cOutReal
               return $ Right $ map realToFrac val
          _ -> return $ Left $ fromIntegral rc
        --putStrLn $ "cOutNbElement " ++ (show $ unsafePerformIO $ peek cOutNbElement)
        --peekArray len cOutReal
    where len = fromIntegral $ length inReal
          endIdx = fromIntegral $ len - 1
          taIntDefault = minBound :: CInt
          cWindow = fromIntegral window
          inReal = map realToFrac series

data MaOpts = MaOpts { -- data MaOpts = MaOpts Int
    maLength :: Int
  } deriving (Eq, Show)

ma :: MaOpts -> V.Vector Double -> V.Vector Double
ma (MaOpts len) vec = vec

-- moving averages without using ta-lib
noTaMa :: MaOpts -> V.Vector Double -> V.Vector Double
noTaMa (MaOpts len) vec = vec

--ma :: MaOpts -> Vector Double -> Vector Double
--ma (MaOpts len) vec = 

main :: IO ()
main = do
	c_ta_init
        let pricesList = [91.500, 94.815, 94.375,
                          95.095, 93.780, 94.625,
                          92.530, 92.750, 90.315,
                          92.470, 96.125, 97.250,
                          98.500, 89.875, 91.000,
                          92.815, 89.155, 89.345,
                          91.625, 89.875, 88.375,
                          87.625, 84.780, 83.000]
        
        let pricesV = V.fromList(pricesList)
            
        let pricesC = map realToFrac pricesList :: [CDouble]
        putStrLn $ show pricesC
        putStrLn ""
        
        result <- ta_ma 5 pricesList
        putStrLn $ show result
        
        --let opts = MaOpts {maLength = 4}
            
	--putStrLn $ show $ c_ta_atr_lookback 87
        
	putStrLn "Elsen!"
	
