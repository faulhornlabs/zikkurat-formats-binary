
-- | Internal hacks

module ZK.Formats.Dummy where

--------------------------------------------------------------------------------

import Data.Word
import Foreign.ForeignPtr
import System.IO.Unsafe

import ZK.Formats.Helpers ( ElementSize(..) )
import ZK.Formats.ForeignArray

--------------------------------------------------------------------------------

{-# NOINLINE dummyForeignPtr #-}
dummyForeignPtr :: ForeignPtr Word8
dummyForeignPtr = unsafePerformIO $ mallocForeignPtrArray 8

dummyForeignArray :: ForeignArray
dummyForeignArray = ForeignArray 0 (ElementSize 0) dummyForeignPtr

--------------------------------------------------------------------------------
