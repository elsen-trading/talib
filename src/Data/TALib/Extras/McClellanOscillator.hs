module Data.TALib.Extras.McClellanOscillator where

import Data.Default
import Data.Vector.Storable.Mutable
import Data.TALib.Timeseries

import Data.TALib.Extras.ExponentialMovingAverage

data McClellanOpts = McClellanOpts {
    mcFastTau :: Double
  , mcSlowTau :: Double
  }

instance Default McClellanOpts where
  def = McClellanOpts 19 23

mcClellan :: Timeseries Double -> Timeseries Double
mcClellan = mcClellan' def

mcClellan' :: McClellanOpts -> Timeseries Double -> Timeseries Double
mcClellan' = undefined
