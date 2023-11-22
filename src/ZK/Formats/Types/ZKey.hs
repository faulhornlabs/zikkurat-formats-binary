
-- | Types corresponding the prover and verifier keys

--------------------------------------------------------------------------------

{-# LANGUAGE StrictData, GeneralizedNewtypeDeriving #-}
module ZK.Formats.Types.ZKey where

--------------------------------------------------------------------------------

import ZK.Formats.ForeignArray 
import ZK.Formats.Primes
import ZK.Formats.Types.Etc

--------------------------------------------------------------------------------

-- | Which version of the Groth16 setup we use
data Flavour
  = JensGroth          -- ^ the version described in the original Groth16 paper
  | Snarkjs            -- ^ the version implemented by Snarkjs
  deriving (Eq,Show)

-- | Special points
data Groth16Header = Groth16Header
  { _curve          :: Curve          -- ^ which elliptic curve
  , _flavour        :: Flavour        -- ^ Groth16 setup flavour
  , _baseFieldCfg   :: FieldConfig    -- ^ the prime field Fp
  , _scalarFieldCfg :: FieldConfig    -- ^ the prime field Fr
  , _nvars          :: Int            -- ^ number of witness variables (including the constant 1 "variable")
  , _npub           :: Int            -- ^ number of public input/output variables (excluding the constant 1)
  , _domainSize     :: Int            -- ^ size of the FFT domain (a power of two)
  , _logDomainSize  :: Int            -- ^ @log2(domainSize)@
  }
  deriving (Eq,Show)

-- | Special points
data SpecPoints = SpecPoints 
  { _alpha1  :: SingletonG1           -- ^ @[alpha]_1@
  , _beta1   :: SingletonG1           -- ^ @[beta]_1@
  , _beta2   :: SingletonG2           -- ^ @[beta]_2@
  , _gamma2  :: SingletonG2           -- ^ @[gamma]_2@
  , _delta1  :: SingletonG1           -- ^ @[delta]_1@
  , _delta2  :: SingletonG2           -- ^ @[delta]_2@
  }
  deriving Show

data VerifierPoints = VerifierPoints
  { _pointsIC   :: G1Array      -- ^ the curve points corresponding to public IO: @[gamma^-1 * ( beta*A_j(tau) + alpha*B_j(tau) + C_j(tau) )]_1@ 
  }
  deriving Show

data ProverPoints = ProverPoints
  { _pointsA    :: G1Array      -- ^ the curve points @[A_j(tau)]_1 in G1@
  , _pointsB1   :: G1Array      -- ^ the curve points @[B_j(tau)]_1 in G1@
  , _pointsB2   :: G2Array      -- ^ the curve points @[B_j(tau)]_2 in G2@
  , _pointsC    :: G1Array      -- ^ the curve points @[delta^-1 * ( beta*A_j(tau) + alpha*B_j(tau) + C_j(tau) )]_1@ in G1
  , _pointsH    :: G1Array      -- ^ the curve points @[delta^-1 * L_{2i+1}(tau)]_1@ in G1
  }
  deriving Show

--------------------------------------------------------------------------------

-- | Which matrix (from the R1CS equation @(Az)*(Bz) = Cz@)
data MatrixSel
  = MatrixA
  | MatrixB
  | MatrixC
  deriving (Eq,Enum,Show)

data ZKeyCoeff = ZKeyCoeff
  { _matrixSel :: MatrixSel
  , _row       :: Int
  , _column    :: Int
  , _value     :: StdFr 
  }
  deriving (Eq,Show)

-- | Verifier key
data VKey = VKey 
  { _vkeySpec    :: SpecPoints
  , _vkeyVPoints :: VerifierPoints
  }
  deriving (Show)

data ZKey = ZKey
  { _zkeyHeader  :: Groth16Header
  , _zkeySpec    :: SpecPoints
  , _zkeyCoeffs  :: [ZKeyCoeff]
  , _zkeyVPoints :: VerifierPoints
  , _zkeyPPoints :: ProverPoints
  }
  deriving (Show)

zkeyCurve :: ZKey -> Curve
zkeyCurve = _curve . _zkeyHeader

--------------------------------------------------------------------------------

extractVKey :: ZKey -> VKey
extractVKey zkey = VKey
  { _vkeySpec    = _zkeySpec    zkey    
  , _vkeyVPoints = _zkeyVPoints zkey 
  }

--------------------------------------------------------------------------------

