
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

import Data.ByteString.Lazy (ByteString) 
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as B

import "binary" Data.Binary.Get

--------------------------------------------------------------------------------

type Msg = String

--------------------------------------------------------------------------------

bindMb :: IO (Maybe a) -> (a -> IO (Maybe b)) -> IO (Maybe b)
bindMb action1 action2 = do
  mb1 <- action1
  case mb1 of
    Nothing -> return Nothing
    Just x  -> action2 x

bindEi :: IO (Either e a) -> (a -> IO (Either e b)) -> IO (Either e b)
bindEi action1 action2 = do
  ei1 <- action1
  case ei1 of
    Left err -> return (Left err)
    Right x  -> action2 x

--------------------------------------------------------------------------------

toIntegerLE :: [Word8] -> Integer
toIntegerLE = go where
  go []     = 0
  go (w:ws) = fromIntegral w + shiftL (go ws) 8

--------------------------------------------------------------------------------

getWord32asInt :: Get Int
getWord32asInt = fromIntegral <$> getWord32le

getInteger :: Int -> Get Integer
getInteger n =  do
  bytes <- getByteString n
  return $ toIntegerLE $ B.unpack bytes

runGetMaybe :: Get a -> L.ByteString -> Maybe a
runGetMaybe action bs = case runGetOrFail action bs of
  Left  (rem,ofs,e)  -> Nothing -- error e -- Nothing
  Right (rem,ofs,x)  -> if L.null rem 
    then Just x
    else Nothing -- error $ "remaining stuff" ++ show ofs

--------------------------------------------------------------------------------

readWord32 :: Handle -> IO Word32
readWord32 h = do
  bytes <- L.hGet h 4
  return $ flip runGet bytes getWord32le 

readWord64 :: Handle -> IO Word64
readWord64 h = do
  bytes <- L.hGet h 8
  return $ flip runGet bytes getWord64le 

readWord32asInt :: Handle -> IO Int
readWord32asInt h = fromIntegral <$> readWord32 h

readWord64asInt :: Handle -> IO Int
readWord64asInt h = fromIntegral <$> readWord64 h

readInteger :: Handle -> Int -> IO Integer
readInteger h k = do
  bytes <- L.hGet h k
  return $ toIntegerLE $ L.unpack bytes

--------------------------------------------------------------------------------
