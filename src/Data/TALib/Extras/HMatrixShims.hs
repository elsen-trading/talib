{- |
Module      :  Data.TALib.Extras.HMatrixShims
Copyright   :  (c) Alberto Ruiz 2012
License     :  BSD3
Maintainer  :  Elsen
Stability   :  provisional

These shims are for converting between Data.Vector.Storable Vectors
and HMatrix's Data.Packed.Vector. Copied from:
https://github.com/albertoruiz/hmatrix/blob/master/examples/vector.hs

Note: HMatrix's vector representation will some day be replaced by
      Data.Vector.Storable
-}

module Data.TALib.Extras.HMatrixShims (fromVector, toVector) where

import Numeric.LinearAlgebra as H
import Data.Packed.Development(unsafeFromForeignPtr, unsafeToForeignPtr)
import Foreign.Storable
import qualified Data.Vector.Storable as V

fromVector :: Storable t => V.Vector t -> H.Vector t
fromVector v = unsafeFromForeignPtr p i n where
    (p,i,n) = V.unsafeToForeignPtr v

toVector :: Storable t => H.Vector t -> V.Vector t
toVector v = V.unsafeFromForeignPtr p i n where
    (p,i,n) = unsafeToForeignPtr v
