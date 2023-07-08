
-- | Parsing the @.wtns@ witness binary file format

{-# LANGUAGE StrictData, PackageImports #-}
module ZK.Formats.Binary.Witness where

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

import ZK.Formats.Binary.Container
import ZK.Formats.ForeignArray
import ZK.Formats.Primes
import ZK.Formats.Helpers

--------------------------------------------------------------------------------

data Witness = Witness 
  { _fieldConfig          :: FieldConfig        -- ^ prime field configuration
  , _numberOfWitnessVars  :: Int                -- ^ number of witness variables (including the special variable "1")
  , _witnessData          :: ForeignArray       -- ^ raw data of the witness variables
  }
  deriving Show

witnessArray :: Witness -> Array Int Integer
witnessArray wtns = readForeignArray id (_witnessData wtns)

witnessList :: Witness -> [Integer]
witnessList = elems . witnessArray

--------------------------------------------------------------------------------

parseWitnessFile :: FilePath -> IO (Either Msg Witness)
parseWitnessFile fname = parseContainerFile fname `bindEi` kont where

  kont :: Container -> IO (Either Msg Witness)
  kont (Container globHdr sections) = case globHdr of
    GlobalHeader _ "wtns" 2 -> worker (sortSectionHeaders sections) 
    _                       -> return (Left "invalid global header (expecting `wtns` file version 2")

  worker :: [SectionHeader] -> IO (Either Msg Witness)
  worker [ sect1@(SectionHeader 1 _ _)
         , sect2@(SectionHeader 2 _ _) ] = do
                        h  <- openBinaryFile fname ReadMode
                        ei <- parseSect1 h sect1 `bindEi` parseSect2 h sect2
                        hClose h
                        return ei
  worker _ = return (Left "expecting two sections with section ids 1,2")

  parseSect1 :: Handle -> SectionHeader -> IO (Either Msg (FieldConfig,Int))
  parseSect1 h (SectionHeader 1 ofs siz) = do
    hSeek h AbsoluteSeek (fromIntegral ofs)
    fieldElemSiz <- readWord32asInt h
    if siz /= 8 + fromIntegral fieldElemSiz
      then return (Left "size of header section does not match the expected value")
      else do
        if (fieldElemSiz < 4 || fieldElemSiz > 256)
          then return (Left "field element size is out of the expected range")
          else do
            prime <- readInteger h fieldElemSiz
            let fieldCfg = FieldConfig (ElementSize fieldElemSiz) prime
            nvars <- readWord32asInt h
            return $ Right (fieldCfg, nvars)

  parseSect2 :: Handle -> SectionHeader -> (FieldConfig,Int) -> IO (Either Msg Witness)
  parseSect2 h (SectionHeader 2 ofs siz) (fieldCfg, nvars) = do
    let fldSize = _bytesPerFieldElement fieldCfg
    hSeek h AbsoluteSeek (fromIntegral ofs)
    if siz /= fromIntegral (fromElementSize fldSize * nvars)
      then return (Left "size of witness section does not match the expected value")
      else do
        fptr <- mallocForeignPtrBytes (fromIntegral siz)
        withForeignPtr fptr $ \ptr -> hGetBuf h ptr (fromIntegral siz)
        return $ Right $ Witness 
          { _fieldConfig          = fieldCfg
          , _numberOfWitnessVars  = nvars
          , _witnessData          = ForeignArray nvars fldSize fptr
          }

--------------------------------------------------------------------------------
