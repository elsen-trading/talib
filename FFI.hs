{-# LANGUAGE ForeignFunctionInterface #-}

module FFI where

import Foreign.C.Types

foreign import ccall unsafe "myfunction"
  myfunction :: Int -> IO (Ptr Int)

foo :: IO (Ptr Int)
foo = myfunction 3
