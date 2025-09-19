
-- | Parsing and exporting the @.ltau@ binary file format 
-- (describing Lagrange basis form of the \"powers of tau\" ceremonies)
--
-- Note: this is custom file format reusing the same container format as @.ptau@ uses
--

--------------------------------------------------------------------------------

{-

file format
===========
 
standard iden3 binary container format.
field elements are in standard representation

Note: there can be several copies of section 4, for differently sized subgroups.

sections:

1: Header
---------
  n8p         : word32    = how many bytes are a field element in Fp
  p           : n8p bytes = the size of the prime field Fp (the base field)
  fileSiz     : word32    = log2(N) where N is the size of setup in this file
  ceremonySiz : word32    = log2(M) where M is the size of the original ceremony (which can be larger)

2: Verifier key
---------------
  g1          : G1        = the generator of the group G1 
  g2          : G2        = the generator of the group G2
  tauG2       : G2        = the point tau*g2 in G2

3: tau*g1 points
----------------
  tauG1       : N points in G1 (coordinates in Montgomery representation)

4: L(tau)*g1 points
-------------------
  log2n       : word32    = size of the subgroup whose Lagrange basis we list
  lTauG1      : (2^log2n) points in G1 = [ L_i(tau) | i<-[0..N-1] ]

-}

--------------------------------------------------------------------------------

{-# LANGUAGE PackageImports, GeneralizedNewtypeDeriving, DeriveFunctor #-}
module ZK.Formats.Binary.Ltau 
  ( module ZK.Formats.Types.Ltau
  , parseLtauFile_
  , parseLtauFile
  , writeLtauFile
  , encodeLagrangeTau
  )
  where

--------------------------------------------------------------------------------

import Data.Char
import Data.Bits
import Data.Word
import Data.Array

import Control.Monad
import Control.Applicative

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Foreign.ForeignPtr
import System.IO

import Data.ByteString.Lazy (ByteString) 
import qualified Data.ByteString.Lazy as L

import "binary" Data.Binary.Get
import "binary" Data.Binary.Builder as Builder

import ZK.Formats.Binary.Container
import ZK.Formats.ForeignArray
import ZK.Formats.Types.Ltau
import ZK.Formats.Types.Ptau
import ZK.Formats.Types
import ZK.Formats.Primes
import ZK.Formats.Helpers
import ZK.Formats.Dummy

--------------------------------------------------------------------------------
-- * Loading @.ltau@ files

parseLtauFile_ :: FilePath -> IO LagrangeTau
parseLtauFile_ fpath = parseLtauFile fpath >>= \ei -> case ei of
  Right ptau -> return ptau
  Left  msg  -> error  msg

parseLtauFile :: FilePath -> IO (Either Msg LagrangeTau)
parseLtauFile fname = parseContainerFile fname `bindEi` kont where

  kont :: Container -> IO (Either Msg LagrangeTau)
  kont (Container globHdr sections) = case globHdr of
    GlobalHeader _ "ltau" 1 -> worker (sortSectionHeaders sections) 
    _                       -> return (Left "invalid global header (expecting `ltau` file version 1")

  ----------------------------------------

  checkSectionHeaders :: [SectionHeader] -> Maybe (SectionHeader, SectionHeader, SectionHeader, [SectionHeader])
  checkSectionHeaders sections = case sortSectionHeaders sections of
    (   sect1@(SectionHeader 1 _ _)
      : sect2@(SectionHeader 2 _ _)   
      : sect3@(SectionHeader 3 _ _) 
      : rest )   -> Just (sect1, sect2, sect3, takeSections 4 rest)
    _            -> Nothing

  takeSections :: Word32 -> [SectionHeader] -> [SectionHeader]
  takeSections k = go where
    go [] = []
    go (sect@(SectionHeader i _ _) : rest) = if k == i then sect : go rest else []

  worker :: [SectionHeader] -> IO (Either Msg LagrangeTau)
  worker sections = case checkSectionHeaders sections of
    Nothing         -> return (Left "expecting at least 4 sections with section ids 1,2,3,4")
    Just (_,_,_,[]) -> return (Left "expecting at least 4 sections with section ids 1,2,3,4")
    Just (sect1,sect2,sect3,sect4s) -> do
                    h  <- openBinaryFile fname ReadMode
                    ei <- parseSect1  h sect1 `bindEi` 
                          parseSect2  h sect2 `bindEi`
                          parseSect3  h sect3 `bindEi`
                          parseSect4s h sect4s
                    hClose h
                    return ei

  parseSect4s :: Handle -> [SectionHeader] -> LagrangeTau -> IO (Either Msg LagrangeTau)
  parseSect4s h []          old = return (Right old)
  parseSect4s h (this:rest) old = parseSect4 h this old `bindEi` parseSect4s h rest

  ----------------------------------------
  -- header section

  parseSect1 :: Handle -> SectionHeader -> IO (Either Msg LagrangeTau)
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
            let dummyVKey = LTauVKey
                  { _ltauG1     = SingletonG1 (G1Array dummyForeignArray)
                  , _ltauG2     = SingletonG2 (G2Array dummyForeignArray)
                  , _ltauTauG2  = SingletonG2 (G2Array dummyForeignArray)
                  }
            return $ Right $ LagrangeTau 
              { _ltauFieldConfig = fieldCfg
              , _ltauCeremonyCfg = ceremonyCfg
              , _ltauVKey        = dummyVKey
              , _ltauPowersOfTau = G1Array dummyForeignArray
              , _lagrangeTable   = IntMap.empty
              }

  ----------------------------------------
  -- data sections

  readForeignArray :: Handle -> ElementSize -> Int -> IO ForeignArray
  readForeignArray h elsize len = do
    let siz = fromElementSize elsize * len
    fptr <- mallocForeignPtrBytes (fromIntegral siz)
    withForeignPtr fptr $ \ptr -> hGetBuf h ptr (fromIntegral siz)
    return (ForeignArray len elsize fptr)

  ----------------------------------------

  parseSect2 :: Handle -> SectionHeader -> LagrangeTau -> IO (Either Msg LagrangeTau)
  parseSect2 h sheader@(SectionHeader 2 ofs siz) ltau = do
    hSeek h AbsoluteSeek (fromIntegral ofs)
    let elsize = _bytesPerFieldElement $ _ltauFieldConfig ltau
    let fldsiz = fromElementSize elsize
    if fldsiz * (2+4+4) /= fromIntegral siz
      then return $ Left $ "size of section 2 does not match the expected value"
      else do
        g1    <- readForeignArray h (ElementSize(2*fldsiz)) 1
        g2    <- readForeignArray h (ElementSize(4*fldsiz)) 1
        tauG2 <- readForeignArray h (ElementSize(4*fldsiz)) 1
        let vkey = LTauVKey
              { _ltauG1     = SingletonG1 (G1Array    g1)
              , _ltauG2     = SingletonG2 (G2Array    g2)
              , _ltauTauG2  = SingletonG2 (G2Array tauG2)
              }
        return $ Right $ ltau { _ltauVKey = vkey }

  parseSect3 :: Handle -> SectionHeader -> LagrangeTau -> IO (Either Msg LagrangeTau)
  parseSect3 h sheader@(SectionHeader 3 ofs siz) ltau = do
    let n = 2 ^ (_logSizeOfPtauFile $ _ltauCeremonyCfg ltau)
    hSeek h AbsoluteSeek (fromIntegral ofs)
    let elsize = _bytesPerFieldElement $ _ltauFieldConfig ltau
    let fldsiz = fromElementSize elsize
    if fldsiz * (2*n) /= fromIntegral siz
      then return $ Left $ "size of section 3 does not match the expected value"
      else do
        tauG1 <- readForeignArray h (ElementSize(2*fldsiz)) n
        return $  Right $ ltau { _ltauPowersOfTau = G1Array tauG1 }

  parseSect4 :: Handle -> SectionHeader -> LagrangeTau -> IO (Either Msg LagrangeTau)
  parseSect4 h sheader@(SectionHeader 4 ofs siz) ltau = do
    hSeek h AbsoluteSeek (fromIntegral ofs)
    log2 <- readWord32asInt h
    if log2 < 1 || log2 > 32 
      then return $ Left $ "invalid subgroup size in section 4"
      else do
        let n = 2^log2
        let elsize = _bytesPerFieldElement $ _ltauFieldConfig ltau
        let fldsiz = fromElementSize elsize
        if 4 + fldsiz*(2*n) /= fromIntegral siz
          then return $ Left $ "size of section 4 does not match the expected value"
          else do
            ltaus <- readForeignArray h (ElementSize(2*fldsiz)) n
            let old = _lagrangeTable ltau
            let new = IntMap.insert log2 (G1Array ltaus) old
            return $ Right $ ltau { _lagrangeTable = new }

--------------------------------------------------------------------------------
-- * Writing @.ltau@ files

writeLtauFile :: FilePath -> LagrangeTau -> IO ()
writeLtauFile fpath ltau = writeContainerFile fpath (encodeLagrangeTau ltau)

encodeLagrangeTau :: LagrangeTau -> Container'
encodeLagrangeTau ltau = container where

  container = Container'
    { _globalHeader' = mkGlobalHeader "ltau" 1
    , _sections'     = [section1,section2,section3] ++ section4s
    }

  fieldcfg = _ltauFieldConfig ltau
  ceremony = _ltauCeremonyCfg ltau
  vkey     = _ltauVKey        ltau

  section1  = mkSection' 1 (putFieldConfig fieldcfg <> putCeremony ceremony)
  section2  = mkSection' 2 (putVKey    $ _ltauVKey ltau)
  section3  = mkSection' 3 (putG1Array $ _ltauPowersOfTau ltau)
  section4s = [ mkSection' 4 (putSection4 ia) | ia <- IntMap.toList (_lagrangeTable ltau) ]

  putSection4 (k,arr) = putIntAsWord32 k <> (putForeignArray $ fromG1Array arr)

  putVKey (LTauVKey g1 g2 taug2) 
    =  putSingletonG1 g1
    <> putSingletonG2 g2
    <> putSingletonG2 g2

--------------------------------------------------------------------------------

