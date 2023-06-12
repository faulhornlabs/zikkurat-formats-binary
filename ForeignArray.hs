
-- | Arrays in raw ForeignPtr data

{-# LANGUAGE BangPatterns, StrictData, PackageImports #-}
module ForeignArray where

--------------------------------------------------------------------------------

import Data.Char
import Data.Bits
import Data.Word
import Data.Array

import Control.Monad
import Control.Applicative

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal

import System.IO
import System.IO.Unsafe

import Helpers

--------------------------------------------------------------------------------

data FieldConfig = FieldConfig 
  { _bytesPerFieldElement :: Int        -- ^ how many bytes per field element
  , _fieldPrime           :: Integer    -- ^ the field prime
  }
  deriving (Eq,Show)

--------------------------------------------------------------------------------

{-# NOINLINE readForeignArray #-}
readForeignArray :: (Integer -> a) -> FieldConfig -> Int -> ForeignPtr Word8 -> Array Int a
readForeignArray f cfg n fptr = unsafePerformIO (getForeignArray f cfg n fptr)

{-# NOINLINE getForeignArray #-}
getForeignArray :: (Integer -> a) -> FieldConfig -> Int -> ForeignPtr Word8 -> IO (Array Int a)
getForeignArray f cfg@(FieldConfig elsiz _) n fptr = do
  withForeignPtr fptr $ \ptr -> do
    list <- worker n ptr 
    return $ listArray (0,n-1) (map f list)
  where
    worker 0  _    = return []
    worker !k !ptr = (:) <$> getIntegerLE ptr <*> worker (k-1) (plusPtr ptr elsiz)

    getIntegerLE :: Ptr Word8 -> IO Integer
    getIntegerLE ptr = toIntegerLE <$> peekArray elsiz ptr

--------------------------------------------------------------------------------
