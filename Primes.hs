
module Primes where

--------------------------------------------------------------------------------

data Curve
  = Bn128                -- ^ bn128 (aka. alt-bn128, BN256, BN254) curve
  | Bls12_381            -- ^ BLS12-381 curve
  | Bls12_377            -- ^ BLS12-377 curve
  | Pallas               -- ^ Pallas curve (Pasta 2-cycle)
  | Vesta                -- ^ Vesta curve (Vesta 2-cycle)
  deriving (Eq,Show)

data OtherPrime
  = Goldilocks           -- ^ @2^64 - 2^32 + 1@
  | BabyBear             -- ^ @2^31 - 2^27 + 1@
  deriving (Eq,Show)

data Prime
  = BaseFieldOf   Curve
  | ScalarFieldOf Curve
  | OtherPrime    OtherPrime
  deriving (Eq,Show)

recognizePrime :: Integer -> Maybe Prime
recognizePrime p 

  | p == bn128_base_p        = Just $ BaseFieldOf   Bn128
  | p == bn128_scalar_r      = Just $ ScalarFieldOf Bn128

  | p == bls12_381_base_p    = Just $ BaseFieldOf   Bls12_381
  | p == bls12_381_scalar_r  = Just $ ScalarFieldOf Bls12_381

  | p == bls12_377_base_p    = Just $ BaseFieldOf   Bls12_377
  | p == bls12_377_scalar_r  = Just $ ScalarFieldOf Bls12_377

  | p == pallas_base_p       = Just $ BaseFieldOf   Pallas
  | p == vesta_base_p        = Just $ BaseFieldOf   Vesta

  | p == goldilocks_p        = Just $ OtherPrime    Goldilocks
  | p == baby_bear_p         = Just $ OtherPrime    BabyBear

  | otherwise                = Nothing

--------------------------------------------------------------------------------
-- BLS12-381 elliptic curve

bls12_381_base_p   = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab 
bls12_381_scalar_r = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001 

--------------------------------------------------------------------------------
-- BLS12-377

bls12_377_base_p   = 0x01ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000001
bls12_377_scalar_r = 0x12ab655e9a2ca55660b44d1e5c37b00159aa76fed00000010a11800000000001

--------------------------------------------------------------------------------
-- BN128 elliptic curve

bn128_base_p   = 21888242871839275222246405745257275088696311157297823662689037894645226208583 
bn128_scalar_r = 21888242871839275222246405745257275088548364400416034343698204186575808495617 

--------------------------------------------------------------------------------
-- Pallas and Vesta

pallas_base_p = 0x40000000000000000000000000000000224698fc094cf91b992d30ed00000001
vesta_base_p  = 0x40000000000000000000000000000000224698fc0994a8dd8c46eb2100000001

--------------------------------------------------------------------------------
-- other primes

goldilocks_p = 2^64 - 2^32 + 1
baby_bear_p  = 2^31 - 2^27 + 1

--------------------------------------------------------------------------------
