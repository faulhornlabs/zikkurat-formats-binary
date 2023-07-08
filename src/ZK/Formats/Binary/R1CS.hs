
-- | Parsing the @.r1cs@ binary file format 
-- (describing arithmetic circuits as rank-1 constraint systems).
--
-- Note: custom gates are not yet implemented.
--

{-# LANGUAGE StrictData, PackageImports, GeneralizedNewtypeDeriving #-}
module ZK.Formats.Binary.R1CS where

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
import ZK.Formats.Primes
import ZK.Formats.Helpers

--------------------------------------------------------------------------------

-- | Note: The witness should be organized in a flat array as:
--
-- > [ 1 | public output | public input | private input | secret witness ]
--
-- with size @nWires@.
--
data WitnessConfig = WitnessConfig
  { _nWires  :: Int                -- ^ total number of wires (or witness variables), including the special \"variable\" constant 1.
  , _nPubOut :: Int                -- ^ number of public outputs
  , _nPubIn  :: Int                -- ^ number of public inputs
  , _nPrvIn  :: Int                -- ^ number of private inputs
  , _nLabels :: Int                -- ^ number of labels
  }
  deriving Show

data R1CS = R1CS
  { _fieldConfig     :: FieldConfig            -- ^ prime field configuration
  , _witnessCfg      :: WitnessConfig          -- ^ witness configuration (public and private inputs)
  , _constraints     :: [Constraint]           -- ^ the list of R1CS constraints
  , _wireToLabelId   :: WireToLabelId          -- ^ mapping of wire indices to label ids
  , _customGates     :: Maybe CustomGates      -- ^ custom gates 
  }
  deriving Show

dummyR1CS :: R1CS
dummyR1CS = R1CS 
  { _fieldConfig     = FieldConfig   (ElementSize 0) 0
  , _witnessCfg      = WitnessConfig 0 0 0 0 0
  , _constraints     = []
  , _wireToLabelId   = WireToLabelId (listArray (0,-1) [])
  , _customGates     = Nothing
  }

-- | An R1CS constraints has the form @A * B = C@ where A,B,C are (affine) linear terms
data Constraint = Constraint
  { _constraintA :: LinComb
  , _constraintB :: LinComb
  , _constraintC :: LinComb
  }
  deriving Show

newtype WireIdx 
  = WireIdx Int
  deriving (Eq,Ord,Show,Num,Ix)

-- | A linear term is a linear combination of witness wariables. Note: the special
-- witness variable with index zero normally denotes the constant 1 \"variable\"
newtype LinComb 
  = LinComb [(WireIdx,Integer)]
  deriving Show

newtype WireToLabelId 
  = WireToLabelId (Array WireIdx Word64)
  deriving Show

data CustomGates = CustomGates
  { _customGateList :: [CustomGateDef]
  , _customGateApps :: [CustomGateApp]
  }
  deriving Show

-- | TODO
type CustomGateDef  = ()
type CustomGateApp  = ()

--------------------------------------------------------------------------------

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
            let fieldCfg = FieldConfig (ElementSize fieldElemSiz) prime
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
  getLinComb (FieldConfig fldSize _) = do
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
        { _fieldConfig   = fieldCfg
        , _witnessCfg    = witnessCfg
        , _constraints   = constraints
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

