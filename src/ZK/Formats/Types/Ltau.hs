
-- | @.ltau@ files are our custom format encoding the Lagrange basis form of
-- a KZG trusted setup, that is, tables of group elements
-- 
-- > [ L_i(tau) | i <- [0..2^{m-1}] ]
-- 
-- where @L_i@ are the Lagrange basis polynomials for a subgroup of size @2^m@,
-- and @tau@ is the secret \"toxic waste\". Differently sized subgroups are 
-- provided. 
--
-- This can be derived from an existing @.ptau@ file using Fourier transformation, 
-- no need for a custom ceremony.
--

{-# LANGUAGE StrictData #-}
module ZK.Formats.Types.Ltau where

--------------------------------------------------------------------------------

import Data.IntMap (IntMap)

import ZK.Formats.ForeignArray
import ZK.Formats.Types.Etc
import ZK.Formats.Types.Ptau
import ZK.Formats.Primes

--------------------------------------------------------------------------------

-- | Notes:
--
-- * G1 elements consist of two bigints, @(x,y)@ coordinates of an elliptic curve point.
-- 
-- * G2 elements should consist of four bigints, @((x1,x2),(y1,y2))@, coordinates of an
--   elliptic curve point over a quadratic field extension (on a twisted curve)
--
-- * Field elements are encoded in Montgomery representation
--
-- * the table of Lagrange bases for different subgroups are indexed by the log2 of
--   the size of the subgroup
--
data LagrangeTau = LagrangeTau 
  { _ltauFieldConfig :: !FieldConfig               -- ^ prime field configuration
  , _ltauCeremonyCfg :: !CeremonyConfig            -- ^ ceremony configuration 
  , _ltauVKey        :: !LTauVKey                  -- ^ verifier key
  , _ltauPowersOfTau :: !G1Array                   -- ^ @[ tau^i*g1 | i<-[0..2^n-1] ]@
  , _lagrangeTable   :: !(IntMap G1Array)          -- ^ table of Lagrange basis SRS-s for differently sized subgroups
  }
  deriving Show

data LTauVKey = LTauVKey
  { _ltauG1     :: !SingletonG1                    -- ^ generator of G1
  , _ltauG2     :: !SingletonG2                    -- ^ generator of G2 
  , _ltauTauG2  :: !SingletonG2                    -- ^ (tau*g2) in G2
  }
  deriving Show

--------------------------------------------------------------------------------
