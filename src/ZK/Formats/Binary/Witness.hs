
-- | Parsing (and exporting) the @.wtns@ witness binary file format

--------------------------------------------------------------------------------

{-

Note: the witness values are a flat array of size `nvars`, organized
in the following order:

    [ 1 | public output | public input | private input | secret witness ]

so we have
    nvars = 1 + pub + secret = 1 + npubout + npubin + nprivin + nsecret

Field elements are encoded in the standard representation.


file format
===========
 
standard iden3 binary container format.
field elements are in standard representation

sections:

1: Header
---------
  n8r     : word32    = how many bytes are a field element in Fr
  r       : n8r bytes = the size of the prime field Fr (the scalar field)
  nVars   : word32    = number of witness variables

2: Witness
----------
  an array of `nVars` field elements in Fr

-}

--------------------------------------------------------------------------------

{-# LANGUAGE StrictData, PackageImports #-}
module ZK.Formats.Binary.Witness 
  ( module ZK.Formats.Types.Witness
  , parseWtnsFile_
  , parseWtnsFile
  , writeWtnsFile
  , encodeWitness
  )
  where

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
import "binary" Data.Binary.Builder

import ZK.Formats.Binary.Container
import ZK.Formats.ForeignArray
import ZK.Formats.Types
import ZK.Formats.Types.Witness
import ZK.Formats.Primes
import ZK.Formats.Helpers

--------------------------------------------------------------------------------

parseWtnsFile_ :: FilePath -> IO Witness
parseWtnsFile_ fpath = parseWtnsFile fpath >>= \ei -> case ei of
  Right wtns -> return wtns
  Left  msg  -> error  msg

parseWtnsFile :: FilePath -> IO (Either Msg Witness)
parseWtnsFile fname = parseContainerFile fname `bindEi` kont where

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
            let fieldCfg = mkFieldConfig (ElementSize fieldElemSiz) prime
            nvars <- readWord32asInt h
            return $ Right (fieldCfg, nvars)

  parseSect2 :: Handle -> SectionHeader -> (FieldConfig,Int) -> IO (Either Msg Witness)
  parseSect2 h (SectionHeader 2 ofs siz) (fieldCfg, nvars) = do
    let fldSize = _bytesPerFieldElement fieldCfg
    hSeek h AbsoluteSeek (fromIntegral ofs)
    if siz /= fromIntegral (fromElementSize fldSize * nvars)
      then return (Left "size of witness section does not match the expected value")
      else do
        farr <- hGetForeignArray h fldSize nvars
        return $ Right $ Witness 
          { _witnessFieldConfig   = fieldCfg
          , _numberOfWitnessVars  = nvars
          , _witnessData          = StdFrArray farr
          }

--------------------------------------------------------------------------------

writeWtnsFile :: FilePath -> Witness -> IO ()
writeWtnsFile fpath witness = writeContainerFile fpath (encodeWitness witness)

encodeWitness :: Witness -> Container'
encodeWitness witness = container where

  container = Container'
    { _globalHeader' = mkGlobalHeader "wtns" 2
    , _sections'     = [section1,section2]
    }

  fieldcfg = _witnessFieldConfig witness
  farray   = fromStdFrArray $ _witnessData witness
  nvars    = _foreignArrayLen farray

  section1 = mkSection' 1 (putFieldConfig fieldcfg <> putWord32le (fromIntegral nvars))
  section2 = mkSection' 2 (putForeignArray $ fromStdFrArray $ _witnessData witness)

--------------------------------------------------------------------------------
