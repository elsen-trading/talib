{-# LANGUAGE ForeignFunctionInterface #-}

module FFI where

import Foreign.C.Types
import Foreign.Ptr

foreign import ccall safe "myfunction"
  myfunction :: CInt -> CInt

sampleFFI :: IO ()
sampleFFI = print (myfunction 42)
-- Calls out to C and prints 43
