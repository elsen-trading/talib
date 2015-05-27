{- |
Module      :  Data.TALib.Extras.ExponentialMovingAverage
Copyright   :  (c) Elsen Inc.
License     :  BSD3
Maintainer  :  Elsen
Stability   :  provisional

A temporary exponential moving average indicator. Until the binding from ta-lib is ready.
Exponential moving average is a convolution of an exponential kernal with another indicator.
It's like a low-pass filter but with a fast onset time. The `tau` parameter controls the
low-pass frequency

In:    |     |
       |     |      ---
       |_____..____/___\...._____

Out:   |     |
       |     |\    ----.
       |_____._\../_____\-.....__
-}

{-# LANGUAGE RecordWildCards #-}

module Data.TALib.Extras.ExponentialMovingAverage (
  tauDays,
  tauSeconds,
  kernel,
  expMovAvg
  ) where

import Foreign.Storable
import Numeric.LinearAlgebra as H
import Numeric.LinearAlgebra.HMatrix as H
import qualified Data.Vector.Storable as Vec

import Data.TALib.Timeseries
import Data.TALib.Extras.HMatrixShims

------------------------------------------------------------------------------
tauDays :: Double -- ^ Length scale in days
        -> TauSI
tauDays = TauSI . (* (3600 * 24)) -- There are 3600 * 24 sec per day

tauSeconds :: Double -> TauSI
tauSeconds = TauSI

expMovAvg :: Kernel Double -> Timeseries Double -> Timeseries Double
expMovAvg (Kernel k kernF) ts@Timeseries{..}
  | kernF == tsFreq =
    ts { tsSamps = H.conv (fromVector k) (fromVector tsSamps) }
  | otherwise       = error $ "Impossible case: Kernel and timeseries "
                      ++ "not on the same timebase. Kernel: " ++ show kernF
                      ++ "  Timeseries: " ++ show tsFreq

type KernelNSamps = Int

------------------------------------------------------------------------------
kernel' :: TauSI         -- ^ Length scale for the exponential kernel
        -> KernelNSamps  -- ^ Number of samps to generate (usuall ~5*tau)
        -> Frequency     -- ^ Sampling freq of timeseries for later convolution
        -> Kernel Double
kernel' lengthScale len (Frequency freq) =
  let -- Exponential kernel drops to 1/e at time tau:
      unscaledKern = Vec.generate len
                     (\i -> exp ( (-1) * (fromIntegral i*freq) /
                                  (unTau lengthScale)))
      -- A kernel's coefficients should sum to 1
      scaledKern   = Vec.map (* (1/ Vec.sum unscaledKern)) unscaledKern
  in  Kernel scaledKern (Frequency freq)

kernel :: TauSI
       -> Frequency
       -> Kernel Double
kernel tau@(TauSI lengthScale) f@(Frequency freq) =
  let kernelNSamps = ceiling $ freq * lengthScale * 5
  in  kernel' tau kernelNSamps f

newtype TauSI = TauSI {
  unTau :: Double
  }

data Kernel a = Kernel {
    kernSamples :: Vec.Vector Double
  , kernFreq    :: Frequency
  }
