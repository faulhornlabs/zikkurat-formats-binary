
-- | The @.wtns@ witness file format

{-# LANGUAGE StrictData, PackageImports #-}
module Witness where

--------------------------------------------------------------------------------

import Data.Char
import Data.Bits
import Data.Word
import Data.Array

import Control.Monad
import Control.Applicative

import Foreign.ForeignPtr

import System.IO

import Data.ByteString.Lazy (ByteString) ; import qualified Data.ByteString.Lazy as L

import "binary" Data.Binary.Get

import Container
import ForeignArray
import Helpers

--------------------------------------------------------------------------------

data Witness = Witness 
  { _fieldConfig          :: FieldConfig        -- ^ prime field configuration
  , _numberOfWitnessVars  :: Int                -- ^ number of witness variables (including the special variable "1")
  , _witnessRawData       :: ForeignPtr Word8   -- ^ raw data of the witness variables
  }
  deriving Show

witnessArray :: Witness -> Array Int Integer
witnessArray wtns = readForeignArray id
  (_fieldConfig          wtns) 
  (_numberOfWitnessVars  wtns)
  (_witnessRawData       wtns)

parseWitnessFile :: FilePath -> IO (Maybe Witness)
parseWitnessFile fname = parseContainerFile fname `bindMb` kont where

  kont :: Container -> IO (Maybe Witness)
  kont (Container globHdr sections) = case globHdr of
    GlobalHeader _ "wtns" 2 -> worker (sortSectionHeaders sections) 
    _                       -> return Nothing

  worker :: [SectionHeader] -> IO (Maybe Witness)
  worker [ sect1@(SectionHeader 1 _ _)
         , sect2@(SectionHeader 2 _ _) ] = do
                        h <- openBinaryFile fname ReadMode
                        mb <- parseSect1 h sect1 `bindMb` parseSect2 h sect2
                        hClose h
                        return mb
  worker _ = return Nothing

  parseSect1 :: Handle -> SectionHeader -> IO (Maybe (FieldConfig,Int))
  parseSect1 h (SectionHeader 1 ofs siz) = do
    hSeek h AbsoluteSeek (fromIntegral ofs)
    fieldElemSiz <- readWord32asInt h
    if siz /= 8 + fromIntegral fieldElemSiz
      then return Nothing
      else do
        prime <- readInteger h fieldElemSiz
        let fieldCfg = FieldConfig fieldElemSiz prime
        nvars <- readWord32asInt h
        return $ Just (fieldCfg, nvars)

  parseSect2 :: Handle -> SectionHeader -> (FieldConfig,Int) -> IO (Maybe Witness)
  parseSect2 h (SectionHeader 2 ofs siz) (fieldCfg, nvars) = do
    hSeek h AbsoluteSeek (fromIntegral ofs)
    if siz /= fromIntegral (_bytesPerFieldElement fieldCfg * nvars)
      then return Nothing
      else do
        fptr <- mallocForeignPtrBytes (fromIntegral siz)
        withForeignPtr fptr $ \ptr -> hGetBuf h ptr (fromIntegral siz)
        return $ Just $ Witness 
          { _fieldConfig          = fieldCfg
          , _numberOfWitnessVars  = nvars
          , _witnessRawData       = fptr
          }

--------------------------------------------------------------------------------
