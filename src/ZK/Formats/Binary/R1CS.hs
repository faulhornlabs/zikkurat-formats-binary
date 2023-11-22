
-- | Parsing the @.r1cs@ binary file format 
-- (describing arithmetic circuits as rank-1 constraint systems).
--
-- Note: custom gates are not yet implemented.
--

--------------------------------------------------------------------------------

{-

file format
===========
 
standard iden3 binary container format.
field elements are in standard representation

sections:

1: Header
---------
  n8r     : word32    = how many bytes are a field element in Fr
  r       : n8r bytes = the size of the prime field Fr (the scalar field)
  nWires  : word32    = number of wires (or witness variables)
  nPubOut : word32    = number of public outputs
  nPubIn  : word32    = number of public inputs
  nPrivIn : word32    = number of private inputs
  nLabels : word64    = number of labels (variable names in the circom source code)

2: Constraints
--------------
  nConstr : word32    = number of constraints
  then an array of constraints:
    A : LinComb
    B : LinComb
    C : LinComb
  meaning `A*B=C`, where LinComb looks like this:
    nTerms : word32     = number of terms
    <an array of terms>
  where a term looks like this:
    idx   : word32      = which witness variable
    coeff : Fr          = the coefficient
    
3: Wire-to-label mapping
------------------------
  <an array of `nWires` many 64 bit words>

4: Custom gates list
--------------------
  ...
  ...

5: Custom gates application
---------------------------
  ...
  ...

--------------------------------------------------------------------------------

-}

{-# LANGUAGE StrictData, PackageImports, GeneralizedNewtypeDeriving #-}
module ZK.Formats.Binary.R1CS 
  ( module ZK.Formats.Types.R1CS
  , parseR1CSFile_
  , parseR1CSFile
  )
  where

--------------------------------------------------------------------------------

import Data.Char
import Data.Bits
import Data.Word
import Data.Array

import Control.Monad
import Control.Applicative

import System.IO

import Data.ByteString.Lazy (ByteString) ; import qualified Data.ByteString.Lazy as L

import "binary" Data.Binary.Get

import ZK.Formats.Binary.Container
import ZK.Formats.ForeignArray ( ElementSize(..) , fromElementSize )
import ZK.Formats.Types.R1CS
import ZK.Formats.Types.Etc
import ZK.Formats.Primes
import ZK.Formats.Helpers

--------------------------------------------------------------------------------

dummyR1CS :: R1CS
dummyR1CS = R1CS 
  { _r1csFieldConfig = dummyFieldConfig
  , _witnessCfg      = WitnessConfig 0 0 0 0 0
  , _constraints     = []
  , _wireToLabelId   = WireToLabelId (listArray (0,-1) [])
  , _customGates     = Nothing
  }

--------------------------------------------------------------------------------

parseR1CSFile_ :: FilePath -> IO R1CS
parseR1CSFile_ fpath = parseR1CSFile fpath >>= \ei -> case ei of
  Right r1cs -> return r1cs
  Left  msg  -> error  msg

parseR1CSFile :: FilePath -> IO (Either Msg R1CS)
parseR1CSFile fname = parseContainerFile fname `bindEi` kont where

  kont :: Container -> IO (Either Msg R1CS)
  kont (Container globHdr sections) = case globHdr of
    GlobalHeader _ "r1cs" 1 -> worker (sortSectionHeaders sections) 
    _                       -> return (Left "invalid global header (expecting `r1cs` file version 1")

  ---------

  worker :: [SectionHeader] -> IO (Either Msg R1CS)
  worker [ sect1@(SectionHeader 1 _ _)
         , sect2@(SectionHeader 2 _ _)   
         , sect3@(SectionHeader 3 _ _) ] = do
                        h  <- openBinaryFile fname ReadMode
                        ei <- parseSect1 h sect1 `bindEi` 
                              parseSect2 h sect2 `bindEi`
                              parseSect3 h sect3
                        hClose h
                        return ei
  worker [ sect1@(SectionHeader 1 _ _)
         , sect2@(SectionHeader 2 _ _)   
         , sect3@(SectionHeader 3 _ _) 
         , sect4@(SectionHeader 4 _ _) 
         , sect5@(SectionHeader 5 _ _) ] = do
                        h  <- openBinaryFile fname ReadMode
                        ei <- parseSect1 h sect1 `bindEi` 
                              parseSect2 h sect2 `bindEi`
                              parseSect3 h sect3 `bindEi`
                              parseSect4 h sect3 `bindEi`
                              parseSect5 h sect3 
                        hClose h
                        return ei
  worker _ = return (Left "expecting 3 or 5 sections with section ids 1,2,3,[4,5]")

  ----------------------------------------
  -- header section

  parseSect1 :: Handle -> SectionHeader -> IO (Either Msg (FieldConfig,WitnessConfig,Int))
  parseSect1 h (SectionHeader 1 ofs siz) = do
    hSeek h AbsoluteSeek (fromIntegral ofs)
    fieldElemSiz <- readWord32asInt h
    if siz /= 32 + fromIntegral fieldElemSiz
      then return (Left "size of header section does not match the expected value")
      else do
        if (fieldElemSiz < 4 || fieldElemSiz > 256)
          then return (Left "field element size is out of the expected range")
          else do
            prime <- readInteger h fieldElemSiz
            let fieldCfg = mkFieldConfig (ElementSize fieldElemSiz) prime
            nwires   <- readWord32asInt h
            npubout  <- readWord32asInt h
            npubin   <- readWord32asInt h
            nprivin  <- readWord32asInt h
            nlabels  <- readWord64asInt h
            nconstr  <- readWord32asInt h
            let wtnsCfg = WitnessConfig
                  { _nWires  = nwires
                  , _nPubOut = npubout
                  , _nPubIn  = npubin
                  , _nPrvIn  = nprivin
                  , _nLabels = nlabels
                  }
            return $ Right (fieldCfg, wtnsCfg, nconstr)

  ----------------------------------------
  -- constraint section

  getLinComb :: FieldConfig -> Get LinComb
  getLinComb fieldcfg = do
    let fldSize = _bytesPerFieldElement fieldcfg
    siz <- getWord32asInt
    LinComb <$> (replicateM siz $ do
      wireidx <- getWord32asInt 
      coeff   <- getInteger (fromElementSize fldSize)
      return (WireIdx wireidx, coeff))

  getConstraint :: FieldConfig -> Get Constraint
  getConstraint fcfg 
     =   Constraint 
     <$> getLinComb fcfg 
     <*> getLinComb fcfg 
     <*> getLinComb fcfg

  parseSect2 :: Handle -> SectionHeader -> (FieldConfig,WitnessConfig,Int) -> IO (Either Msg R1CS)
  parseSect2 h (SectionHeader 2 ofs siz) (fieldCfg, witnessCfg, nConstr) = do
    hSeek h AbsoluteSeek (fromIntegral ofs)
    bs <- L.hGet h (fromIntegral siz)
    return $ case runGetMaybe (replicateM nConstr (getConstraint fieldCfg)) bs of
      Nothing          -> Left "parsing of the constraint section fauled"
      Just constraints -> Right $ dummyR1CS
        { _r1csFieldConfig = fieldCfg
        , _witnessCfg      = witnessCfg
        , _constraints     = constraints
        }

  ----------------------------------------
  -- wire to label mapping section

  parseSect3 :: Handle -> SectionHeader -> R1CS -> IO (Either Msg R1CS)
  parseSect3 h (SectionHeader 3 ofs siz) old = do
    let nwires = _nWires (_witnessCfg old)
    if 8*nwires /= fromIntegral siz 
      then return $ Left "unexpected size for the `wire-to-label-id' section"
      else do
        hSeek h AbsoluteSeek (fromIntegral ofs)
        bs <- L.hGet h (fromIntegral siz)
        return $ case runGetMaybe (replicateM nwires getWord64le) bs of
          Nothing    -> Left "parsing of the `wire-to-label-id' section fauled"
          Just list  -> Right $ old 
            { _wireToLabelId = WireToLabelId (listArray (0,WireIdx (nwires-1)) list) }

  ----------------------------------------
  -- custom gates list section

  parseSect4 :: Handle -> SectionHeader -> R1CS -> IO (Either Msg R1CS)
  parseSect4 h (SectionHeader 4 ofs siz) old = return (Right old)

  ----------------------------------------
  -- custom gate application section

  parseSect5 :: Handle -> SectionHeader -> R1CS -> IO (Either Msg R1CS)
  parseSect5 h (SectionHeader 5 ofs siz) old = return (Right old)

--------------------------------------------------------------------------------

