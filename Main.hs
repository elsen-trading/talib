import Foreign.C.Types
import Foreign.Ptr

-- Compile with:
-- ghc MovingAverages.hs -l ta_lib

foreign import ccall safe "ta_common.h TA_Initialize"
  c_ta_init :: IO ()

-- just to make sure we're calling the C code
foreign import ccall safe "ta_func.h TA_ATR_Lookback"
  c_ta_atr_lookback :: CInt -> CInt

data MaOpts = MaOpts { -- data MaOpts = MaOpts Int
    maLength :: Int
  } deriving (Eq, Show)

--ma :: MaOpts -> Vector Double -> Vector Double
--ma (MaOpts len) vec = 

main :: IO ()
main = do
	c_ta_init
	putStrLn (show (c_ta_atr_lookback 87))
	putStrLn "Elsen!"
	
