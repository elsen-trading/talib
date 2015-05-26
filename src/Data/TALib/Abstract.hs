{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.TALib.Abstract (
  call,
  example,
) where

import Foreign.Ptr
import Foreign.C.Types

import Foreign.LibFFI
import System.Posix.DynamicLinker

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.ForeignPtr
import Control.Applicative

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Signature = Signature
  { itypes :: [Type]
  , otypes :: [Type]
  } deriving (Eq, Ord, Show)

data Type
  = TInt
  | TDouble
  | TArray Type
  | TPtr Type
  deriving (Eq, Ord, Show)

data Val
  = VInt CInt
  | VDouble CDouble
  | VPtr IntPtr
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- FFI
-------------------------------------------------------------------------------

retty :: RetType (Ptr ())
retty = retPtr retVoid

call
  :: Signature                   -- A talib signature
  -> FunPtr a                    -- A C function pointer
  -> [Val]                       -- A list of sum type of arguments
  -> IO (Either String (Ptr ())) -- A pointer to the TSOutput
call (Signature itypes _) f vals
  | typechecks = Right <$> callFFI f retty (fmap fromType vals)
  | otherwise  = pure (Left "Function called with improper arguments")
  where
    -- Do all the types line up?
    typechecks = and (zipWith typeMatch vals itypes)

typeMatch :: Val -> Type -> Bool
typeMatch (VInt {})    (TInt)     = True
typeMatch (VDouble {}) (TDouble)  = True
typeMatch (VPtr {})    (TArray _) = True
typeMatch (VPtr {})    (TPtr _ )  = True
typeMatch _ _                     = False

-- http://hackage.haskell.org/package/libffi-0.1/docs/Foreign-LibFFI-Types.html
fromType :: Val -> Arg
fromType (VInt n)    = argCInt n
fromType (VDouble n) = argCDouble n
fromType (VPtr n)    = argPtr (intPtrToPtr n)

-------------------------------------------------------------------------------
-- Example
-------------------------------------------------------------------------------

-- Example of calling function pointers with runtime type information and
-- signatures generated dynamically as runtime values.

ma_sig :: Signature
ma_sig = Signature
  { itypes = [
    TInt,           -- startIdx
    TInt,           -- endIdx
    TArray TDouble, -- input array
    TInt,           -- option
    TInt,           -- option
    TPtr TInt,      -- outBegIdx
    TPtr TInt,      -- outNBElement
    TPtr TDouble    -- output array
  ]
  , otypes = []
  }

example :: IO ()
example = do
  let vs = V.fromList ([1,3,5,2,1,2,5,9,6] :: [CInt])
  vec <- V.thaw vs

  VM.unsafeWith vec $ \ptr -> do
    -- Get the function pointer
    c_ta_ma <- dlsym Default "TA_MA"

    -- Construct the dynamic runtime arguments
    let args = [VInt 0, VInt 100, VPtr (ptrToIntPtr ptr)]

    -- Call the function pointer with the runtime signature and arguments
    ret <- call ma_sig c_ta_ma args
    print ret
