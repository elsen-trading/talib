{- This module is mostly a shim to get tests running.
|  I'm not sure what the high-level API should look like,
|  but the stuff in this module might be a place to start:
|  Timeseries
-}


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.TALib.Timeseries where

import qualified Data.Vector.Storable as Vec

data Timeseries a = Timeseries {
    unTS     :: Vec.Vector a
  -- , t0       :: UTCTime
  -- , sampRate :: (Somewhere Stephen has a sum type for stock sampling intervals..)
  } deriving (Eq, Show)
