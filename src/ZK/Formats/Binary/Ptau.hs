
-- | Parsing the @.ptau@ binary file format (describing \"powers of tau\" ceremonies)
--
-- Note: only parsing the result of the ceremony is implemented so far,
-- the contributors, history and proofs are not.
--

{-# LANGUAGE PackageImports, GeneralizedNewtypeDeriving, DeriveFunctor #-}
module ZK.Formats.Binary.Ptau where

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

data CeremonyConfig = CeremonyConfig
  { _logSizeOfPtauFile :: !Int      -- ^ @n@ where @2^n@ is the characteristic size of the data contained in the file
  , _logSizeOfCeremony :: !Int      -- ^ @n@ where @2^n@ is the original size of the ceremony (eg. 28 for the Hermez ceremony)
  }
  deriving Show

-- | Notes:
--
-- * G1 elements should consist of two bigints, @(x,y)@ coordinates of an elliptic curve point.
-- 
-- * G2 elements should usually consist of four bigints, @((x1,x2),(y1,y2))@, coordinates of an
--   elliptic curve point over a quadratic field extension (on a twisted curve)
--
-- * Field elements are encoded in Montgomery representation
--
data PowersOfTau = PowersOfTau
  { _fieldConfig     :: !FieldConfig           -- ^ prime field configuration
  , _ceremonyCfg     :: !CeremonyConfig        -- ^ ceremony configuration 
  , _tauG1           ::  G1Array               -- ^ @[         (tau^k) * g1 | k <- [0 ..2*N-2] ]@    
  , _tauG2           ::  G2Array               -- ^ @[         (tau^k) * g2 | k <- [0 ..  N-1] ]@
  , _alphaTauG1      ::  G1Array               -- ^ @[ alpha * (tau^k) * g1 | k <- [0 ..  N-1] ]@
  , _betaTauG1       ::  G1Array               -- ^ @[ beta  * (tau^k) * g1 | k <- [0 ..  N-1] ]@
  , _betaG2          ::  G2Array               -- ^ @[ beta * g2 ]@
  }
  deriving Show

-- | An array of curve points on the curve G1. Note: the ForeignArray is an array
-- of field elements, of size @2*N@.
newtype G1Array 
  = G1Array ForeignArray 
  deriving Show

-- | An array of curve points on the curve G2. Note: the ForeignArray is an array
-- of prime field elements, of size @4*N@.
newtype G2Array 
  = G2Array ForeignArray 
  deriving Show

--------------------------------------------------------------------------------

data G1 a = G1 !a !a         deriving (Eq,Show,Functor)
data G2 a = G2 !(a,a) !(a,a) deriving (Eq,Show,Functor)

readG1Array :: G1Array -> [G1 Integer]
readG1Array (G1Array farr) = toG1 $ readForeignArrayAsList id farr where
  toG1 []         = []
  toG1 (x:y:rest) = G1 x y : toG1 rest
  toG1 _          = error "readG1Array: should not happen"

readG2Array :: G2Array -> [G2 Integer]
readG2Array (G2Array farr) = toG2 $ readForeignArrayAsList id farr where
  toG2 []                 = []
  toG2 (x1:x2:y1:y2:rest) = G2 (x1,x2) (y1,y2) : toG2 rest
  toG2 _                  = error "readG2Array: should not happen"

--------------------------------------------------------------------------------

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
            let fieldCfg = FieldConfig (ElementSize fieldElemSiz) prime
            sizeFile     <- readWord32asInt h
            sizeCeremony <- readWord32asInt h
            let ceremonyCfg = CeremonyConfig
                  { _logSizeOfPtauFile = sizeFile
                  , _logSizeOfCeremony = sizeCeremony
                  }
            let dummyForeignArray = error "missing ForeignArray (this should not happen)"
            return $ Right $ PowersOfTau
              { _fieldConfig = fieldCfg
              , _ceremonyCfg = ceremonyCfg
              , _tauG1       = dummyForeignArray
              , _tauG2       = dummyForeignArray
              , _alphaTauG1  = dummyForeignArray
              , _betaTauG1   = dummyForeignArray
              , _betaG2      = dummyForeignArray
              }

  ----------------------------------------
  -- data sections

  parseGenericSection :: Int -> Int -> Handle -> SectionHeader -> PowersOfTau -> IO (Either Msg ForeignArray)
  parseGenericSection nfldPerEntry expectedLen h (SectionHeader _ ofs siz) ptau = do
    hSeek h AbsoluteSeek (fromIntegral ofs)
    let fldsiz = fromElementSize $ _bytesPerFieldElement $ _fieldConfig ptau
    if nfldPerEntry * expectedLen * fldsiz /= fromIntegral siz
      then return $ Left $ "size of header section does not match the expected value " ++
             "(expecting " ++ show expectedLen ++ " group elements, each consisting of " ++ show nfldPerEntry ++ " field elements)"
      else do
        fptr <- mallocForeignPtrBytes (fromIntegral siz)
        withForeignPtr fptr $ \ptr -> hGetBuf h ptr (fromIntegral siz)
        return $ Right $ ForeignArray (nfldPerEntry * expectedLen) (ElementSize fldsiz) fptr

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

