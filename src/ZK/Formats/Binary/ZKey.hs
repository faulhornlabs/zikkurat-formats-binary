
-- | Parsing the @.zkey@ binary file format 
-- (containing circuit-specific Groth16 proving and verifying keys)
--
-- Note: contributor history is not implemented
--

--------------------------------------------------------------------------------

{-

file format
===========

standard iden3 binary container format.

field elements are in Montgomery representation, except for the coefficients
which for some reason are double Montgomery encoded... (and unlike the 
`.wtns` and `.r1cs` files which use the standard representation...)

sections
========

1: Header
---------
  prover_type : word32 (Groth16 = 0x0001)

2: Groth16-specific header
--------------------------
  n8p     : word32    = how many bytes are a field element in Fp
  p       : n8p bytes = the size of the prime field Fp (the base field)
  n8r     : word32    = how many bytes are a field element in Fr
  r       : n8p bytes = the size of the prime field Fr (the scalar field)
  nvars   : word32    = number of witness variables
  npub    : word32    = number of public variables (public input/output)
  domSize : word32    = domain size (power of two)
  alpha1  : G1        = [alpha]_1
  beta1   : G1        = [beta]_1
  beta2   : G2        = [beta]_2
  gamma2  : G2        = [gamma]_2
  delta1  : G1        = [delta]_1
  delta2  : G2        = [delta_2]

3: IC
-----
  the curve points (corresponding to public input) required by the verifier
  [gamma^-1 * ( beta*A_j(tau) + alpha*B_j(tau) + C_j(tau) )]_1 
  length = 2 * n8p * (npub + 1) = (npub+1) G1 points

4: Coeffs
---------
  ncoeffs : words32 = number of entries
  The nonzero coefficients in the A,B R1CS matrices (that is, sparse representation)
  Remark: since we now that (A*witness).(B*witness) = C.witness
  (12+n8r) bytes per entry:
    m     : word32         = which matrix (0=A, 1=B)
    c     : word32         = which row, from 0..domSize-1
    s     : word32         = which column, from 0..nvars-1
    value : Fr (n8r bytes)
 
  for each such entry, we add `value * witness[c]` to the `i`-th element of
  the corresponding column vector (meaning `A*witness` and `B*witness), then
  compute (C*witness)[i] = (A*witness)[i] * (B*witness)[i]
  These 3 column vectors is all we need in the proof generation.

 WARNING! It appears that the values here are *doubly Montgomery encoded* (??!)

5: PointsA
----------
  the curve points [A_j(tau)]_1 in G1
  length = 2 * n8p * nvars = nvars G1 points

6: PointsB1
-----------
  the curve points [B_j(tau)]_1 in G1
  length = 2 * n8p * nvars = nvars G1 points

7: PointsB2
-----------
  the curve points [B_j(tau)]_2 in G2
  length = 4 * n8p * nvars = nvars G2 points

8: PointsC
----------
  the curve points [delta^-1 * (beta*A_j(tau) + alpha*B_j(tau) + C_j(tau))]_1 in G1
  length = 2 * n8p * (nvars - npub - 1) = (nvars-npub-1) G1 points

9: PointsH
----------
  what normally should be the curve points `[ delta^-1 * tau^i * Z(tau) ]_1`
  HOWEVER, in the Snarkjs implementation, they are different; namely
  `[ delta^-1 * L_{2i+1} (tau) ]_1` where L_k are Lagrange polynomials
  on the refined (double sized) domain. 
  See <https://geometry.xyz/notebook/the-hidden-little-secret-in-snarkjs>
  length = 2 * n8p * domSize = domSize G1 points

10: Contributions
-----
  ??? (but not required for proving, only for checking that the `.zkey` file is valid)

-}

--------------------------------------------------------------------------------

{-# LANGUAGE StrictData, PackageImports, GeneralizedNewtypeDeriving #-}
module ZK.Formats.Binary.ZKey 
  ( module ZK.Formats.Types.ZKey
  , parseZKeyFile_
  , parseZKeyFile
  ) where

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
import ZK.Formats.ForeignArray 
import ZK.Formats.Dummy
import ZK.Formats.Primes
import ZK.Formats.Types.Etc
import ZK.Formats.Types.ZKey
import ZK.Formats.Helpers

--------------------------------------------------------------------------------

dummyProverPoints :: ProverPoints
dummyProverPoints = ProverPoints
  { _pointsA  = (G1Array dummyForeignArray)
  , _pointsB1 = (G1Array dummyForeignArray)
  , _pointsB2 = (G2Array dummyForeignArray)
  , _pointsC  = (G1Array dummyForeignArray)
  , _pointsH  = (G1Array dummyForeignArray)
  }

dummyVerifierPoints :: VerifierPoints
dummyVerifierPoints = VerifierPoints (G1Array dummyForeignArray)

mkDummyZkey :: (Groth16Header, SpecPoints) -> ZKey
mkDummyZkey (header,spec) = ZKey
  { _zkeyHeader  = header
  , _zkeySpec    = spec
  , _zkeyCoeffs  = []
  , _zkeyVPoints = dummyVerifierPoints
  , _zkeyPPoints = dummyProverPoints
  }

--------------------------------------------------------------------------------

parseZKeyFile_ :: FilePath -> IO ZKey
parseZKeyFile_ fpath = parseZKeyFile fpath >>= \ei -> case ei of
  Right zkey -> return zkey
  Left  msg  -> error  msg

parseZKeyFile :: FilePath -> IO (Either Msg ZKey)
parseZKeyFile fname = parseContainerFile fname `bindEi` kont where

  kont :: Container -> IO (Either Msg ZKey)
  kont (Container globHdr sections) = case globHdr of
    GlobalHeader _ "zkey" 1 -> worker (take 9 $ sortSectionHeaders sections) 
    _                       -> return (Left "invalid global header (expecting `zkey` file version 1")

  ---------

  worker :: [SectionHeader] -> IO (Either Msg ZKey)
  worker [ sect1@(SectionHeader  1 _ _)
         , sect2@(SectionHeader  2 _ _)   
         , sect3@(SectionHeader  3 _ _) 
         , sect4@(SectionHeader  4 _ _) 
         , sect5@(SectionHeader  5 _ _)
         , sect6@(SectionHeader  6 _ _) 
         , sect7@(SectionHeader  7 _ _) 
         , sect8@(SectionHeader  8 _ _)
         , sect9@(SectionHeader  9 _ _) ] = do 
                        h  <- openBinaryFile fname ReadMode
                        ei <- parseSect1 h sect1 `bindEi` 
                              parseSect2 h sect2 `bindEi`
                              parseSect3 h sect3 `bindEi`
                              parseSect4 h sect4 `bindEi`
                              parseSect5 h sect5 `bindEi`
                              parseSect6 h sect6 `bindEi`
                              parseSect7 h sect7 `bindEi`
                              parseSect8 h sect8 `bindEi`
                              parseSect9 h sect9 
                        hClose h
                        return ei
  worker _ = return (Left "expecting at least 9 sections with section ids 1..9")

  ----------------------------------------
  -- global header section

  parseSect1 :: Handle -> SectionHeader -> IO (Either Msg ())
  parseSect1 h (SectionHeader 1 ofs siz) = do
    if siz /= 4
      then return (Left "size of header section (1) does not match the expected value 4")
      else do
        hSeek h AbsoluteSeek (fromIntegral ofs)
        protocol <- readWord32asInt h
        if protocol /= 1
          then return (Left $ "not a Groth16 zkey file (protoocol=" ++ show protocol ++ ")")
          else return (Right ())

  ----------------------------------------
  -- Groth16 header section

  readFieldConfig :: Handle -> IO (Either Msg FieldConfig)
  readFieldConfig h = do
    fieldElemSiz <- readWord32asInt h
    if (fieldElemSiz < 4 || fieldElemSiz > 256)
      then return (Left "field element size is out of the expected range")
      else do    
        prime <- readInteger h fieldElemSiz
        let fieldCfg = mkFieldConfig (ElementSize fieldElemSiz) prime
        return (Right fieldCfg)

  readG1 :: FieldConfig -> Handle -> IO SingletonG1
  readG1 fp h = SingletonG1 <$> (G1Array <$> hGetForeignArray h elsize 1) where
    elsize = doubleElementSize (_bytesPerFieldElement fp)

  readG2 :: FieldConfig -> Handle -> IO SingletonG2
  readG2 fp h = SingletonG2 <$> (G2Array <$> hGetForeignArray h elsize 1) where
    elsize = quadrupleElementSize (_bytesPerFieldElement fp)

  readSpecPoints :: FieldConfig -> Handle -> IO SpecPoints
  readSpecPoints fp h = do
    alpha1  <- readG1 fp h
    beta1   <- readG1 fp h
    beta2   <- readG2 fp h
    gamma2  <- readG2 fp h
    delta1  <- readG1 fp h
    delta2  <- readG2 fp h
    return $ SpecPoints 
      { _alpha1  = alpha1 
      , _beta1   = beta1  
      , _beta2   = beta2  
      , _gamma2  = gamma2 
      , _delta1  = delta1 
      , _delta2  = delta2 
      }

  parseSect2 :: Handle -> SectionHeader -> () -> IO (Either Msg ZKey)
  parseSect2 h (SectionHeader 2 ofs siz) _ = do
    hSeek h AbsoluteSeek (fromIntegral ofs)
    eiBaseField   <- readFieldConfig h 
    eiScalarField <- readFieldConfig h
    case (eiBaseField, eiScalarField) of
      (Left err, _)        -> return (Left err)
      (_, Left err)        -> return (Left err)
      (Right fp, Right fr) -> do
        let np = fromElementSize $ _bytesPerFieldElement fp
        let nr = fromElementSize $ _bytesPerFieldElement fr
        if fromIntegral siz /= np + nr + 20 + 3*6*np
          then return (Left "size of Groth16 header section (2) does not match the expected value")
          else do
            nvars    <- readWord32asInt h
            npub     <- readWord32asInt h
            domSize  <- readWord32asInt h
            let logDomSize = ceilingLog2 (fromIntegral domSize)
            if (2^logDomSize /= domSize) 
              then return (Left "domain size is not a power of two")
              else do
                spec <- readSpecPoints fr h
                case recognizeCurve (_fieldPrime fp) (_fieldPrime fr) of
                  Nothing    -> return (Left "cannot recognize elliptic curve from the field sizes")
                  Just curve -> do 
                    let header = Groth16Header 
                          { _curve          = curve
                          , _flavour        = Snarkjs
                          , _baseFieldCfg   = fp
                          , _scalarFieldCfg = fr
                          , _nvars          = nvars
                          , _npub           = npub
                          , _domainSize     = domSize
                          , _logDomainSize  = logDomSize
                          }             
                    return $ Right $ mkDummyZkey (header, spec)

  ----------------------------------------
  -- coefficient section

  getMatrixSel :: Get MatrixSel
  getMatrixSel = do
    x <- getWord32asInt 
    case x of
      0 -> return MatrixA
      1 -> return MatrixB

  getCoeff :: FieldConfig -> Get ZKeyCoeff
  getCoeff fr = do
    sel   <- getMatrixSel
    row   <- getWord32asInt 
    col   <- getWord32asInt 
    coeff <- getInteger (fromElementSize $ _bytesPerFieldElement fr)
    return $ ZKeyCoeff sel row col (convertFromDoubleMontgomeryFr fr $ DoubleMontgomeryFr coeff)  

  parseSect4 :: Handle -> SectionHeader -> ZKey -> IO (Either Msg ZKey)
  parseSect4 h (SectionHeader 4 ofs siz) oldzkey = do
    let fr = _scalarFieldCfg $ _zkeyHeader oldzkey
    hSeek h AbsoluteSeek (fromIntegral ofs)
    ncoeffs  <- readWord32asInt h
    let nr = fromElementSize (_bytesPerFieldElement fr)
    if fromIntegral siz /= 4 + ncoeffs * (12 + nr)
      then return $ Left "unexpected size for the `coeffs` section (4)" 
      else do
        bs <- L.hGet h (fromIntegral siz - 4)
        return $ case runGetMaybe (replicateM ncoeffs (getCoeff fr)) bs of
          Nothing          -> Left "parsing of the `coeffs` section (4) fauled"
          Just coeffs      -> Right $ oldzkey { _zkeyCoeffs = coeffs } 

  ----------------------------------------
  -- IC points (verifier public input)

  parseSect3 :: Handle -> SectionHeader -> ZKey -> IO (Either Msg ZKey)
  parseSect3 h (SectionHeader 3 ofs siz) oldzkey = do
    let header = _zkeyHeader oldzkey
    let npub = _npub header
    let fp   = _baseFieldCfg header
    let np   = fromElementSize $ _bytesPerFieldElement fp
    if (npub+1) * 2 * np /= fromIntegral siz 
      then return $ Left "unexpected size for the `pointsIC` section (3)"
      else do
        hSeek h AbsoluteSeek (fromIntegral ofs)
        farr <- hGetForeignArray h (ElementSize $ 2*np) (npub+1)
        return $ Right $ oldzkey { _zkeyVPoints = VerifierPoints (G1Array farr) }

  ----------------------------------------

  parseSect5 :: Handle -> SectionHeader -> ZKey -> IO (Either Msg ZKey)
  parseSect5 h (SectionHeader 5 ofs siz) oldzkey = do
    let header = _zkeyHeader oldzkey
    let nvars  = _nvars header
    let fp     = _baseFieldCfg header
    let np     = fromElementSize $ _bytesPerFieldElement fp
    if nvars * 2 * np /= fromIntegral siz 
      then return $ Left "unexpected size for the `pointsA` section (5)"
      else do
        hSeek h AbsoluteSeek (fromIntegral ofs)
        farr <- hGetForeignArray h (ElementSize $ 2*np) (nvars)
        let oldppoints = _zkeyPPoints oldzkey
        return $ Right $ oldzkey { _zkeyPPoints = (oldppoints { _pointsA = G1Array farr }) }

  ----------------------------------------

  parseSect6 :: Handle -> SectionHeader -> ZKey -> IO (Either Msg ZKey)
  parseSect6 h (SectionHeader 6 ofs siz) oldzkey = do
    let header = _zkeyHeader oldzkey
    let nvars  = _nvars header
    let fp     = _baseFieldCfg header
    let np     = fromElementSize $ _bytesPerFieldElement fp
    if nvars * 2 * np /= fromIntegral siz 
      then return $ Left "unexpected size for the `pointsB1` section (6)"
      else do
        hSeek h AbsoluteSeek (fromIntegral ofs)
        farr <- hGetForeignArray h (ElementSize $ 2*np) (nvars)
        let oldppoints = _zkeyPPoints oldzkey
        return $ Right $ oldzkey { _zkeyPPoints = (oldppoints { _pointsB1 = G1Array farr }) }

  ----------------------------------------

  parseSect7 :: Handle -> SectionHeader -> ZKey -> IO (Either Msg ZKey)
  parseSect7 h (SectionHeader 7 ofs siz) oldzkey = do
    let header = _zkeyHeader oldzkey
    let nvars  = _nvars header
    let fp     = _baseFieldCfg header
    let np     = fromElementSize $ _bytesPerFieldElement fp
    if nvars * 4 * np /= fromIntegral siz 
      then return $ Left "unexpected size for the `pointsB2` section (7)"
      else do
        hSeek h AbsoluteSeek (fromIntegral ofs)
        farr <- hGetForeignArray h (ElementSize $ 4*np) (nvars)
        let oldppoints = _zkeyPPoints oldzkey
        return $ Right $ oldzkey { _zkeyPPoints = (oldppoints { _pointsB2 = G2Array farr }) }

  ----------------------------------------

  parseSect8 :: Handle -> SectionHeader -> ZKey -> IO (Either Msg ZKey)
  parseSect8 h (SectionHeader 8 ofs siz) oldzkey = do
    let header = _zkeyHeader oldzkey
    let nvars  = _nvars header
    let npub   = _npub  header
    let fp     = _baseFieldCfg header
    let np     = fromElementSize $ _bytesPerFieldElement fp
    if (nvars - npub - 1) * 2 * np /= fromIntegral siz 
      then return $ Left "unexpected size for the `pointsC` section (8)"
      else do
        hSeek h AbsoluteSeek (fromIntegral ofs)
        farr <- hGetForeignArray h (ElementSize $ 2*np) (nvars - npub - 1)
        let oldppoints = _zkeyPPoints oldzkey
        return $ Right $ oldzkey { _zkeyPPoints = (oldppoints { _pointsC = G1Array farr }) }

  ----------------------------------------

  parseSect9 :: Handle -> SectionHeader -> ZKey -> IO (Either Msg ZKey)
  parseSect9 h (SectionHeader 9 ofs siz) oldzkey = do
    let header = _zkeyHeader oldzkey
    let domsiz = _domainSize header
    let fp     = _baseFieldCfg header
    let np     = fromElementSize $ _bytesPerFieldElement fp
    if domsiz * 2 * np /= fromIntegral siz 
      then return $ Left "unexpected size for the `pointsH` section (9)"
      else do
        hSeek h AbsoluteSeek (fromIntegral ofs)
        farr <- hGetForeignArray h (ElementSize $ 2*np) domsiz
        let oldppoints = _zkeyPPoints oldzkey
        return $ Right $ oldzkey { _zkeyPPoints = (oldppoints { _pointsH = G1Array farr }) }

--------------------------------------------------------------------------------

