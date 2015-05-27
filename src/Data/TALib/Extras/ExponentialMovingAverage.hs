module Data.TALib.Extras.ExponentialMovingAverage (
  tauDays,
  tauSecs,
  kernel,
  expMovAvg
  ) where

import Foreign.Storable
import Numerical.LinearAlgebra as H
import qualified Data.Vector.Storable as Vec

------------------------------------------------------------------------------
tauDays :: Double -- ^ Length scale in days
        -> TauSI
tauDays nDays = TauSI . (* (3600 * 24)) -- There are 3600 * 24 sec per day


expMovAvg :: Kernel -> Timeseries -> Timeseries
expMovAvg (Kernel k) ts =
  H.conv (fromVector k) (fromVector (samples ts))

type KernelNSamps = Int

------------------------------------------------------------------------------
kernel :: TauSI         -- ^ Length scale for the exponential kernel
       -> KernelNSamps  -- ^ Number of samps to generate (usuall ~5*tau)
       -> Frequency     -- ^ Sampling freq of timeseries for later convolution
       -> Kernel
kernel lengthScale len (Frequency freq) =
  let -- Exponential kernel drops to 1/e at time tau:
      unscaledKern = Vec.generate len
                     (\i -> exp ( (-1) * (i*freq) / tau))
      -- A kernel's coefficients should sum to 1
      scaledKern   = (* (1/ Vec.sum unscaledKern)) <$> unscaledKern
  in  Kernel scaledKern (Frequency freq)

newtype TauSI = TauSI {
  unTau :: Double
  }

data Kernel a = Kernel {
  kernSamples :: Vec.Vector Double
  kernFreq    :: Frequency
  }
