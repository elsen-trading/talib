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

type CTA1Int1 = CInt        -- startIdx
             -> CInt        -- endIdx
             -> Ptr CDouble -- input array
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

-- CTA3Int1 - 3 input arrays, an Int option, and 1 output array
type CTA3Int1 = CInt        -- startIdx
             -> CInt        -- endIdx
             -> Ptr CDouble -- input array
             -> Ptr CDouble -- input array
             -> Ptr CDouble -- input array
             -> CInt        -- option
             -> Ptr CInt    -- outBegIdx
             -> Ptr CInt    -- outNBElement
             -> Ptr CDouble -- output array
             -> IO CInt

-- CTA4_1 - 4 input arrays, no options, and 1 output array
type CTA4_1 = CInt        -- startIdx
           -> CInt        -- endIdx
           -> Ptr CDouble -- input array
           -> Ptr CDouble -- input array
           -> Ptr CDouble -- input array
           -> Ptr CDouble -- input array
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

type TS1Int = [Double]
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

type TS3Int = [Double]
           -> [Double]
           -> [Double]
           -> Int
           -> TSOutput

type TS4_ = [Double]
         -> [Double]
         -> [Double]
         -> [Double]
         -> TSOutput

-- A TSFun is a C ta-lib function, and the corresponding inputs (input data and options)
data TSFun = TA1IntInt1 CTA1IntInt1 [Double] Int Int
           | TA1Int1 CTA1Int1 [Double] Int
           | TA2_1 CTA2_1 [Double] [Double]
           | TA4IntInt1 CTA4IntInt1 [Double] [Double] [Double] [Double] Int Int
           | TA2Int2 CTA2Int2 [Double] [Double] Int
           | TA4Int1 CTA4Int1 [Double] [Double] [Double] [Double] Int
           | TA3Int1 CTA3Int1 [Double] [Double] [Double] Int
           | TA4_1 CTA4_1 [Double] [Double] [Double] [Double]

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
          len = fromIntegral $ length $ case tsfun of
            TA1IntInt1 _ in1 _ _       -> in1
            TA1Int1 _ in1 _            -> in1
            TA2_1 _ in1 _              -> in1
            TA4IntInt1 _ in1 _ _ _ _ _ -> in1
            TA2Int2 _ in1 _ _          -> in1
            TA4Int1 _ in1 _ _ _ _      -> in1
            TA3Int1 _ in1 _ _ _        -> in1
            TA4_1 _ in1 _ _ _          -> in1
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

-- TA-Lib : Technical Analysis Library

foreign import ccall unsafe "ta_common.h TA_Initialize"
  c_ta_init :: IO ()

-- AD                  Chaikin A/D Line

-- input: inHigh[], inLow[], inClose[], inVolume[];
-- options: none
-- output: outReal[]
foreign import ccall unsafe "ta_func.h TA_AD"
  c_ta_ad :: CTA4_1

ta_ad :: TS4_
ta_ad inHigh inLow inClose inVolume
    = ta_lib (TA4_1 c_ta_ad inHigh inLow inClose inVolume)

-- ADOSC               Chaikin A/D Oscillator

-- input:   inHigh[], inLow[], inClose[], inVolume[];
-- options: int optInFastPeriod (2-100000), int optInSlowPeriod (2-100000);
-- output:  outReal[]
foreign import ccall unsafe "ta_func.h TA_ADOSC"
  c_ta_adosc :: CTA4IntInt1

ta_adosc :: TS4IntInt
ta_adosc inHigh inLow inClose inVolume optInFastPeriod optInSlowPeriod
    = ta_lib (TA4IntInt1 c_ta_adosc inHigh inLow inClose inVolume optInFastPeriod optInSlowPeriod)

-- ADX                 Average Directional Movement Index

-- input: inHigh[], inLow[], inClose[];
-- options: int optInTimePeriod (2-100000)
-- output: outReal[]
foreign import ccall unsafe "ta_func.h TA_ADX"
  c_ta_adx :: CTA3Int1

ta_adx :: TS3Int
ta_adx inHigh inLow inClose optInTimePeriod
    = ta_lib (TA3Int1 c_ta_adx inHigh inLow inClose optInTimePeriod)

-- ADXR                Average Directional Movement Index Rating
-- APO                 Absolute Price Oscillator
-- AROON               Aroon

-- input:   inHigh[], inLow[];
-- options: int optInTimePeriod (2-100000);
-- output:  outAroonDown[], outAroonUp[]
foreign import ccall unsafe "ta_func.h TA_AROON"
  c_ta_aroon :: CTA2Int2

ta_aroon :: TS2Int
ta_aroon inHigh inLow optInTimePeriod
    = ta_lib (TA2Int2 c_ta_aroon inHigh inLow optInTimePeriod)

-- AROONOSC            Aroon Oscillator
-- ATR                 Average True Range

-- input: inHigh[], inLow[], inClose[];
-- options: int optInTimePeriod (1-100000)
-- output: outReal[]
foreign import ccall unsafe "ta_func.h TA_ATR"
  c_ta_atr :: CTA3Int1

ta_atr :: TS3Int
ta_atr inHigh inLow inClose optInTimePeriod
    = ta_lib (TA3Int1 c_ta_atr inHigh inLow inClose optInTimePeriod)

-- AVGPRICE            Average Price
-- BBANDS              Bollinger Bands
-- BETA                Beta
-- BOP                 Balance Of Power
-- CCI                 Commodity Channel Index
-- CDL2CROWS           Two Crows
-- CDL3BLACKCROWS      Three Black Crows
-- CDL3INSIDE          Three Inside Up/Down
-- CDL3LINESTRIKE      Three-Line Strike 
-- CDL3OUTSIDE         Three Outside Up/Down
-- CDL3STARSINSOUTH    Three Stars In The South
-- CDL3WHITESOLDIERS   Three Advancing White Soldiers
-- CDLABANDONEDBABY    Abandoned Baby
-- CDLADVANCEBLOCK     Advance Block
-- CDLBELTHOLD         Belt-hold
-- CDLBREAKAWAY        Breakaway
-- CDLCLOSINGMARUBOZU  Closing Marubozu
-- CDLCONCEALBABYSWALL Concealing Baby Swallow
-- CDLCOUNTERATTACK    Counterattack
-- CDLDARKCLOUDCOVER   Dark Cloud Cover
-- CDLDOJI             Doji
-- CDLDOJISTAR         Doji Star
-- CDLDRAGONFLYDOJI    Dragonfly Doji
-- CDLENGULFING        Engulfing Pattern
-- CDLEVENINGDOJISTAR  Evening Doji Star
-- CDLEVENINGSTAR      Evening Star
-- CDLGAPSIDESIDEWHITE Up/Down-gap side-by-side white lines
-- CDLGRAVESTONEDOJI   Gravestone Doji
-- CDLHAMMER           Hammer
-- CDLHANGINGMAN       Hanging Man
-- CDLHARAMI           Harami Pattern
-- CDLHARAMICROSS      Harami Cross Pattern
-- CDLHIGHWAVE         High-Wave Candle
-- CDLHIKKAKE          Hikkake Pattern
-- CDLHIKKAKEMOD       Modified Hikkake Pattern
-- CDLHOMINGPIGEON     Homing Pigeon
-- CDLIDENTICAL3CROWS  Identical Three Crows
-- CDLINNECK           In-Neck Pattern
-- CDLINVERTEDHAMMER   Inverted Hammer
-- CDLKICKING          Kicking
-- CDLKICKINGBYLENGTH  Kicking - bull/bear determined by the longer marubozu
-- CDLLADDERBOTTOM     Ladder Bottom
-- CDLLONGLEGGEDDOJI   Long Legged Doji
-- CDLLONGLINE         Long Line Candle
-- CDLMARUBOZU         Marubozu
-- CDLMATCHINGLOW      Matching Low
-- CDLMATHOLD          Mat Hold
-- CDLMORNINGDOJISTAR  Morning Doji Star
-- CDLMORNINGSTAR      Morning Star
-- CDLONNECK           On-Neck Pattern
-- CDLPIERCING         Piercing Pattern
-- CDLRICKSHAWMAN      Rickshaw Man
-- CDLRISEFALL3METHODS Rising/Falling Three Methods
-- CDLSEPARATINGLINES  Separating Lines
-- CDLSHOOTINGSTAR     Shooting Star
-- CDLSHORTLINE        Short Line Candle
-- CDLSPINNINGTOP      Spinning Top
-- CDLSTALLEDPATTERN   Stalled Pattern
-- CDLSTICKSANDWICH    Stick Sandwich
-- CDLTAKURI           Takuri (Dragonfly Doji with very long lower shadow)
-- CDLTASUKIGAP        Tasuki Gap
-- CDLTHRUSTING        Thrusting Pattern
-- CDLTRISTAR          Tristar Pattern
-- CDLUNIQUE3RIVER     Unique 3 River
-- CDLUPSIDEGAP2CROWS  Upside Gap Two Crows
-- CDLXSIDEGAP3METHODS Upside/Downside Gap Three Methods
-- CMO                 Chande Momentum Oscillator
-- CORREL              Pearson's Correlation Coefficient (r)
-- DEMA                Double Exponential Moving Average
-- DX                  Directional Movement Index
-- EMA                 Exponential Moving Average
-- HT_DCPERIOD         Hilbert Transform - Dominant Cycle Period
-- HT_DCPHASE          Hilbert Transform - Dominant Cycle Phase
-- HT_PHASOR           Hilbert Transform - Phasor Components
-- HT_SINE             Hilbert Transform - SineWave
-- HT_TRENDLINE        Hilbert Transform - Instantaneous Trendline
-- HT_TRENDMODE        Hilbert Transform - Trend vs Cycle Mode
-- KAMA                Kaufman Adaptive Moving Average
-- LINEARREG           Linear Regression
-- LINEARREG_ANGLE     Linear Regression Angle
-- LINEARREG_INTERCEPT Linear Regression Intercept
-- LINEARREG_SLOPE     Linear Regression Slope
-- MA                  All Moving Average

-- input: inReal[]; options: int optInTimePeriod, int optInMAType; output: outReal[]
foreign import ccall unsafe "ta_func.h TA_MA"
  c_ta_ma :: CTA1IntInt1

ta_ma :: TS1IntInt
ta_ma inReal optInTimePeriod optInMAType
    = ta_lib (TA1IntInt1 c_ta_ma inReal optInTimePeriod optInMAType)

-- MACD                Moving Average Convergence/Divergence
-- MACDEXT             MACD with controllable MA type
-- MACDFIX             Moving Average Convergence/Divergence Fix 12/26
-- MAMA                MESA Adaptive Moving Average
-- MAX                 Highest value over a specified period
-- MAXINDEX            Index of highest value over a specified period
-- MEDPRICE            Median Price

-- input: inHigh[], inLow[]; options: none; output: outReal[]
foreign import ccall unsafe "ta_func.h TA_MEDPRICE"
  c_ta_medprice :: CTA2_1

ta_medprice :: TS2_
ta_medprice inHigh inLow
    = ta_lib (TA2_1 c_ta_medprice inHigh inLow)

-- MFI                 Money Flow Index

-- input: inHigh[], inLow[], inClose[], inVolume[];
-- options: int optInTimePeriod (2-100000);
-- output: outReal[]
foreign import ccall unsafe "ta_func.h TA_MFI"
  c_ta_mfi :: CTA4Int1

ta_mfi :: TS4Int
ta_mfi inHigh inLow inClose inVolume optInTimePeriod
    = ta_lib (TA4Int1 c_ta_mfi inHigh inLow inClose inVolume optInTimePeriod)

-- MIDPOINT            MidPoint over period
-- MIDPRICE            Midpoint Price over period
-- MIN                 Lowest value over a specified period
-- MININDEX            Index of lowest value over a specified period
-- MINMAX              Lowest and highest values over a specified period
-- MINMAXINDEX         Indexes of lowest and highest values over a specified period
-- MINUS_DI            Minus Directional Indicator
-- MINUS_DM            Minus Directional Movement
-- MOM                 Momentum
-- NATR                Normalized Average True Range
-- OBV                 On Balance Volume

-- input: inReal[], inVolume[]; options: none; output: outReal[]
foreign import ccall unsafe "ta_func.h TA_OBV"
  c_ta_obv :: CTA2_1

ta_obv :: TS2_
ta_obv inReal inVolume
    = ta_lib (TA2_1 c_ta_obv inReal inVolume)

-- PLUS_DI             Plus Directional Indicator
-- PLUS_DM             Plus Directional Movement
-- PPO                 Percentage Price Oscillator
-- ROC                 Rate of change : ((price/prevPrice)-1)*100
-- ROCP                Rate of change Percentage: (price-prevPrice)/prevPrice
-- ROCR                Rate of change ratio: (price/prevPrice)
-- ROCR100             Rate of change ratio 100 scale: (price/prevPrice)*100
-- RSI                 Relative Strength Index

-- input: inReal[]; options: int optInTimePeriod (2-100000); output: outReal[]
foreign import ccall unsafe "ta_func.h TA_RSI"
    c_ta_rsi :: CTA1Int1

ta_rsi :: TS1Int
ta_rsi inReal optInTimePeriod
    = ta_lib (TA1Int1 c_ta_rsi inReal optInTimePeriod)

-- SAR                 Parabolic SAR
-- SAREXT              Parabolic SAR - Extended
-- SMA                 Simple Moving Average
-- STDDEV              Standard Deviation
-- STOCH               Stochastic
-- STOCHF              Stochastic Fast
-- STOCHRSI            Stochastic Relative Strength Index
-- SUM                 Summation
-- T3                  Triple Exponential Moving Average (T3)
-- TEMA                Triple Exponential Moving Average
-- TRANGE              True Range
-- TRIMA               Triangular Moving Average
-- TRIX                1-day Rate-Of-Change (ROC) of a Triple Smooth EMA
-- TSF                 Time Series Forecast
-- TYPPRICE            Typical Price
-- ULTOSC              Ultimate Oscillator
-- VAR                 Variance
-- WCLPRICE            Weighted Close Price
-- WILLR               Williams' %R
-- WMA                 Weighted Moving Average

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
        
        
        
        
        
        
        
        