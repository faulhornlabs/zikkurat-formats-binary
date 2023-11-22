
-- | Types related to the powers-of-tau ceremony
--
-- Note: only parsing the result of the ceremony is implemented so far,
-- the contributors, history and proofs are not.
--

{-# LANGUAGE StrictData #-}
module ZK.Formats.Types.Ptau where

--------------------------------------------------------------------------------

import ZK.Formats.ForeignArray
import ZK.Formats.Types.Etc
import ZK.Formats.Primes

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
  { _potFieldConfig  :: !FieldConfig           -- ^ prime field configuration
  , _ceremonyCfg     :: !CeremonyConfig        -- ^ ceremony configuration 
  , _tauG1           ::  G1Array               -- ^ @[         (tau^k) * g1 | k <- [0 ..2*N-2] ]@    
  , _tauG2           ::  G2Array               -- ^ @[         (tau^k) * g2 | k <- [0 ..  N-1] ]@
  , _alphaTauG1      ::  G1Array               -- ^ @[ alpha * (tau^k) * g1 | k <- [0 ..  N-1] ]@
  , _betaTauG1       ::  G1Array               -- ^ @[ beta  * (tau^k) * g1 | k <- [0 ..  N-1] ]@
  , _betaG2          ::  G2Array               -- ^ @[ beta * g2 ]@
  }
  deriving Show

--------------------------------------------------------------------------------

