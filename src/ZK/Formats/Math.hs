
-- | Just enough math to be able to convert between standard and Montgomery 
-- representation of prime fields

{-# LANGUAGE BangPatterns #-}
module ZK.Formats.Math where

--------------------------------------------------------------------------------

import Data.Bits

import Data.IORef
import System.IO.Unsafe

import qualified Data.Map as Map

--------------------------------------------------------------------------------

add :: Integer -> Integer -> Integer -> Integer 
add !p !a !b = mod (a+b) p

mul :: Integer -> Integer -> Integer -> Integer 
mul !p !a !b = mod (a*b) p

pow :: Integer -> Integer -> Integer -> Integer
pow !p !x !e = go 1 x e where
  go :: Integer -> Integer -> Integer -> Integer 
  go !a _   0 = a
  go !a !y !e = go a' y' e' where
    a' = if (testBit e 0) then (a*y) else a
    y' = mul p y y
    e' = shiftR e 1

calcInv :: Integer -> Integer -> Integer 
calcInv !p !a = pow p a (p-2)

--------------------------------------------------------------------------------
-- * cache multiplicative inverses

{-# NOINLINE theInvTable #-}
theInvTable :: IORef (Map.Map (Integer,Integer) Integer)
theInvTable = unsafePerformIO $ newIORef Map.empty

{-# NOINLINE lookupInvIO #-}
lookupInvIO :: Integer -> Integer -> IO Integer
lookupInvIO !p !x = do
  table <- readIORef theInvTable
  case Map.lookup (p,x) table of
    Just y  -> return y
    Nothing -> do
      let y = calcInv p x
      writeIORef theInvTable (Map.insert (p,x) y table)
      return y

{-# NOINLINE inv #-}
inv :: Integer -> Integer -> Integer
inv !p !x = unsafePerformIO (lookupInvIO p x)

--------------------------------------------------------------------------------
