
{-# LANGUAGE StrictData, PackageImports #-}
module Helpers where

--------------------------------------------------------------------------------

import Data.Char
import Data.Bits
import Data.Word
import Data.Ord

import Control.Monad
import Control.Applicative

import System.IO

import Data.ByteString.Lazy (ByteString) ; import qualified Data.ByteString.Lazy as L

import "binary" Data.Binary.Get

--------------------------------------------------------------------------------

bindMb :: IO (Maybe a) -> (a -> IO (Maybe b)) -> IO (Maybe b)
bindMb action1 action2 = do
  mb1 <- action1
  case mb1 of
    Nothing -> return Nothing
    Just x  -> action2 x

--------------------------------------------------------------------------------

toIntegerLE :: [Word8] -> Integer
toIntegerLE = go where
  go []     = 0
  go (w:ws) = fromIntegral w + shiftL (go ws) 8

--------------------------------------------------------------------------------

readWord32 :: Handle -> IO Word32
readWord32 h = do
  bytes <- L.hGet h 4
  return $ flip runGet bytes getWord32le 

readWord32asInt :: Handle -> IO Int
readWord32asInt h = fromIntegral <$> readWord32 h

readWord64 :: Handle -> IO Word64
readWord64 h = do
  bytes <- L.hGet h 4
  return $ flip runGet bytes getWord64le 

readInteger :: Handle -> Int -> IO Integer
readInteger h k = do
  bytes <- L.hGet h k
  return $ toIntegerLE $ L.unpack bytes

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
