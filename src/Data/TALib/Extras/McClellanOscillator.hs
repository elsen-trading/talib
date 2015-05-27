module Data.TALib.Extras.McClellanOscillator where

import Data.Default
import Data.Vector.Storable.Mutable
import Data.Vector.TALib.Timeseries

import Data.Vector.TALib.Extras.ExponentialMovingAverage

data McClellanOpts = MCOpts {
    mcFastTau :: Double
  , mcSlowTau :: Double
  }

instance Default McClellanOpts where
  def = McClellanOpts 19 23

mcClellan :: Timeseries -> Timeseries
mcClellan = mcClellan' def

mcClellan' :: McClellanOpts -> Timeseries -> Timeseries
mcClellan' = undefined
