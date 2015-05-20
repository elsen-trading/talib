{-# LANGUAGE ForeignFunctionInterface #-}

module FFI where

import Foreign.C.Types
import Foreign.Ptr

foreign import ccall safe "myfunction"
  myfunction :: CInt -> CInt -> CInt

