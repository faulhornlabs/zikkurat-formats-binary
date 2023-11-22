
-- | Parsing the @.ptau@ binary file format (describing \"powers of tau\" ceremonies)
--
-- Note: only parsing the result of the ceremony is implemented so far,
-- the contributors, history and proofs are not.
--

{-# LANGUAGE PackageImports, GeneralizedNewtypeDeriving, DeriveFunctor #-}
module ZK.Formats.Binary.Ptau 
  ( module ZK.Formats.Types.Ptau
  , parsePtauFile_
  , parsePtauFile
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

import ZK.Formats.Binary.Container
import ZK.Formats.ForeignArray
import ZK.Formats.Types.Ptau
import ZK.Formats.Types
import ZK.Formats.Primes
import ZK.Formats.Helpers

--------------------------------------------------------------------------------

parsePtauFile_ :: FilePath -> IO PowersOfTau
parsePtauFile_ fpath = parsePtauFile fpath >>= \ei -> case ei of
  Right ptau -> return ptau
  Left  msg  -> error  msg

parsePtauFile :: FilePath -> IO (Either Msg PowersOfTau)
parsePtauFile fname = parseContainerFile fname `bindEi` kont where

  kont :: Container -> IO (Either Msg PowersOfTau)
  kont (Container globHdr sections) = case globHdr of
    GlobalHeader _ "ptau" 1 -> worker (sortSectionHeaders sections) 
    _                       -> return (Left "invalid global header (expecting `ptau` file version 1")

  ----------------------------------------

  worker :: [SectionHeader] -> IO (Either Msg PowersOfTau)
  worker ( sect1@(SectionHeader 1 _ _)
         : sect2@(SectionHeader 2 _ _)   
         : sect3@(SectionHeader 3 _ _) 
         : sect4@(SectionHeader 4 _ _) 
         : sect5@(SectionHeader 5 _ _) 
         : sect6@(SectionHeader 6 _ _) 
         : _ )  = do
                    h  <- openBinaryFile fname ReadMode
                    ei <- parseSect1 h sect1 `bindEi` 
                          parseSect2 h sect2 `bindEi`
                          parseSect3 h sect3 `bindEi`
                          parseSect4 h sect4 `bindEi`
                          parseSect5 h sect5 `bindEi`
                          parseSect6 h sect6 
                    hClose h
                    return ei
  worker _ = return (Left "expecting at least 6 sections with section ids 1,2,3,4,5,6")

  ----------------------------------------
  -- header section

  parseSect1 :: Handle -> SectionHeader -> IO (Either Msg PowersOfTau)
  parseSect1 h (SectionHeader 1 ofs siz) = do
    hSeek h AbsoluteSeek (fromIntegral ofs)
    fieldElemSiz <- readWord32asInt h
    if siz /= 12 + fromIntegral fieldElemSiz
      then return (Left "size of header section does not match the expected value")
      else do
        if (fieldElemSiz < 4 || fieldElemSiz > 256)
          then return (Left "field element size is out of the expected range")
          else do
            prime <- readInteger h fieldElemSiz
            let fieldCfg = mkFieldConfig (ElementSize fieldElemSiz) prime
            sizeFile     <- readWord32asInt h
            sizeCeremony <- readWord32asInt h
            let ceremonyCfg = CeremonyConfig
                  { _logSizeOfPtauFile = sizeFile
                  , _logSizeOfCeremony = sizeCeremony
                  }
            let dummyForeignArray = error "missing ForeignArray (this should not happen)"
            return $ Right $ PowersOfTau
              { _potFieldConfig  = fieldCfg
              , _ceremonyCfg     = ceremonyCfg
              , _tauG1           = dummyForeignArray
              , _tauG2           = dummyForeignArray
              , _alphaTauG1      = dummyForeignArray
              , _betaTauG1       = dummyForeignArray
              , _betaG2          = dummyForeignArray
              }

  ----------------------------------------
  -- data sections

  parseGenericSection :: Int -> Int -> Handle -> SectionHeader -> PowersOfTau -> IO (Either Msg ForeignArray)
  parseGenericSection nfldPerEntry expectedLen h (SectionHeader _ ofs siz) ptau = do
    hSeek h AbsoluteSeek (fromIntegral ofs)
    let elsize = _bytesPerFieldElement $ _potFieldConfig ptau
    let fldsiz = fromElementSize elsize
    if nfldPerEntry * expectedLen * fldsiz /= fromIntegral siz
      then return $ Left $ "size of header section does not match the expected value " ++
             "(expecting " ++ show expectedLen ++ " group elements, each consisting of " ++ show nfldPerEntry ++ " field elements)"
      else do
        fptr <- mallocForeignPtrBytes (fromIntegral siz)
        withForeignPtr fptr $ \ptr -> hGetBuf h ptr (fromIntegral siz)
        return $ Right $ ForeignArray (expectedLen) (ElementSize $ nfldPerEntry * fldsiz) fptr

  ----------------------------------------

  parseSect2 :: Handle -> SectionHeader -> PowersOfTau -> IO (Either Msg PowersOfTau)
  parseSect2 h sheader@(SectionHeader 2 ofs siz) ptau = do
    let n = 2 ^ (_logSizeOfPtauFile $ _ceremonyCfg ptau)
    bindEi_ (parseGenericSection 2 (2*n-1) h sheader ptau) $ \farr -> do
      return $ ptau { _tauG1 = G1Array farr }

  parseSect3 :: Handle -> SectionHeader -> PowersOfTau -> IO (Either Msg PowersOfTau)
  parseSect3 h sheader@(SectionHeader 3 ofs siz) ptau = do
    let n = 2 ^ (_logSizeOfPtauFile $ _ceremonyCfg ptau)
    bindEi_ (parseGenericSection 4 n h sheader ptau) $ \farr -> do
      return $ ptau { _tauG2 = G2Array farr }

  parseSect4 :: Handle -> SectionHeader -> PowersOfTau -> IO (Either Msg PowersOfTau)
  parseSect4 h sheader@(SectionHeader 4 ofs siz) ptau = do
    let n = 2 ^ (_logSizeOfPtauFile $ _ceremonyCfg ptau)
    bindEi_ (parseGenericSection 2 n h sheader ptau) $ \farr -> do
      return $ ptau { _alphaTauG1 = G1Array farr }

  parseSect5 :: Handle -> SectionHeader -> PowersOfTau -> IO (Either Msg PowersOfTau)
  parseSect5 h sheader@(SectionHeader 5 ofs siz) ptau = do
    let n = 2 ^ (_logSizeOfPtauFile $ _ceremonyCfg ptau)
    bindEi_ (parseGenericSection 2 n h sheader ptau) $ \farr -> do
      return $ ptau { _betaTauG1 = G1Array farr }

  parseSect6 :: Handle -> SectionHeader -> PowersOfTau -> IO (Either Msg PowersOfTau)
  parseSect6 h sheader@(SectionHeader 6 ofs siz) ptau = do
    bindEi_ (parseGenericSection 4 1 h sheader ptau) $ \farr -> do
      return $ ptau { _betaG2 = G2Array farr }

--------------------------------------------------------------------------------

