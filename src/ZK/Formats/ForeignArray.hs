
-- | Arrays (of big integers) in raw ForeignPtr data.
--
-- Note: usually we don't really want to decode these into Haskell data structure,
-- except for debugging \/ printing \/ logging purposes, because for performance
-- reasons we will have wrapped C code operating on them.
--

{-# LANGUAGE BangPatterns, StrictData, PackageImports #-}
module ZK.Formats.ForeignArray where

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

import ZK.Formats.Helpers

--------------------------------------------------------------------------------

-- | Size of an element in bytes
newtype ElementSize 
  = ElementSize Int
  deriving (Eq,Show)

fromElementSize :: ElementSize -> Int
fromElementSize (ElementSize k) = k

data ForeignArray = ForeignArray 
  { _foreignArrayLen      :: Int                -- ^ length of the array (measured in elements)
  , _foreignArrayElemSize :: ElementSize        -- ^ size of an element in bytes
  , _foreignArrayPtr      :: ForeignPtr Word8   -- ^ pointer to the raw data
  }
  deriving (Eq,Show)

--------------------------------------------------------------------------------

-- | Reads the data in a @ForeignArray@ (consisting of bigints) into a normal Haskell array
readForeignArray :: (Integer -> a) -> ForeignArray -> Array Int a
readForeignArray f (ForeignArray len elsize fptr) = readRawForeignArray f elsize len fptr

readForeignArrayAsList :: (Integer -> a) -> ForeignArray -> [a]
readForeignArrayAsList f farr = elems (readForeignArray f farr)

getForeignArray :: (Integer -> a) -> ForeignArray -> IO (Array Int a)
getForeignArray f (ForeignArray len elsize fptr) = getRawForeignArray f elsize len fptr

--------------------------------------------------------------------------------

{-# NOINLINE readRawForeignArray #-}
readRawForeignArray :: (Integer -> a) -> ElementSize -> Int -> ForeignPtr Word8 -> Array Int a
readRawForeignArray f cfg n fptr = unsafePerformIO (getRawForeignArray f cfg n fptr)

{-# NOINLINE getRawForeignArray #-}
getRawForeignArray :: (Integer -> a) -> ElementSize -> Int -> ForeignPtr Word8 -> IO (Array Int a)
getRawForeignArray f (ElementSize elsiz) n fptr = do
  withForeignPtr fptr $ \ptr -> do
    list <- worker n ptr 
    return $ listArray (0,n-1) (map f list)
  where
    worker 0  _    = return []
    worker !k !ptr = (:) <$> getIntegerLE ptr <*> worker (k-1) (plusPtr ptr elsiz)

    getIntegerLE :: Ptr Word8 -> IO Integer
    getIntegerLE ptr = toIntegerLE <$> peekArray elsiz ptr

--------------------------------------------------------------------------------
