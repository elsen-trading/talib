{- This module is mostly a shim to get tests running.
|  I'm not sure what the high-level API should look like,
|  but the stuff in this module might be a place to start:
|  Timeseries
-}


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.TALib.Timeseries where

import           Data.Time
import           Data.Vector.Storable as Vec

data Timeseries a = Timeseries {
    tsSamps :: Vec.Vector a
  , tsT0    :: UTCTime
  , tsFreq  :: Frequency
  } deriving (Eq, Show)

hz :: Double -> Frequency
hz = Frequency

perMinute :: Double -> Frequency
perMinute = Frequency . (/60)

perHour :: Double -> Frequency
perHour = Frequency . (/ 3600)

perDay :: Double -> Frequency
perDay = Frequency . (/ (3600 * 24))

newtype Frequency = Frequency {
  unFrequency :: Double
  } deriving (Eq, Show)
