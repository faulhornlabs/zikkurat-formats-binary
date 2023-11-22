
{-# LANGUAGE StrictData, DeriveFunctor #-}
module ZK.Formats.Types.Etc where

--------------------------------------------------------------------------------

import Data.Array

import ZK.Formats.ForeignArray
import ZK.Formats.Math as Math

--------------------------------------------------------------------------------
-- * Field configuration

-- | This is how the field prime is represented in the iden3 binary formats.
data FieldConfig = FieldConfig 
  { _bytesPerFieldElement :: !ElementSize    -- ^ how many bytes per field element
  , _wordsPerFieldElement :: !WordSize       -- ^ how many 64-bit words per field element
  , _fieldPrime           :: !Integer        -- ^ the field prime
  }
  deriving (Eq,Show)

mkFieldConfig :: ElementSize -> Integer -> FieldConfig
mkFieldConfig elsize prime = FieldConfig elsize (elementSizeToWordSize elsize) prime

dummyFieldConfig :: FieldConfig 
dummyFieldConfig = FieldConfig (ElementSize 0) (WordSize 0) 0

newtype WordSize 
  = WordSize Int
  deriving (Eq,Ord,Show)

elementSizeToWordSize :: ElementSize -> WordSize
elementSizeToWordSize (ElementSize n) = WordSize ((n + 7) `div` 8)

--------------------------------------------------------------------------------
-- * Foreign arrays

-- | An array of field elements in @F_r@, in standard representation
newtype StdFrArray
  = StdFrArray { fromStdFrArray :: ForeignArray }
  deriving Show

-- | An array of field elements in @F_r@, in Motgomery representation representation
newtype MontFrArray
  = MontFrArray { fromMontFrArray :: ForeignArray }
  deriving Show

-- | An array of curve points on the curve G1. Note: the ForeignArray is an array
-- of field elements, of size @2*N@. Field elements are encoded in Montgomery 
-- representation.
newtype G1Array 
  = G1Array { fromG1Array :: ForeignArray }
  deriving Show

-- | An array of curve points on the curve G2. Note: the ForeignArray is an array
-- of prime field elements, of size @4*N@. Field elements are encoded in Montgomery 
-- representation.
newtype G2Array 
  = G2Array { fromG2Array :: ForeignArray }
  deriving Show

--------------------------------------------------------------------------------

-- | A single G1 element
newtype SingletonG1 
  = SingletonG1 { fromSingletonG1 :: G1Array }
  deriving Show

-- | A single G2 element
newtype SingletonG2
  = SingletonG2 { fromSingletonG2 :: G2Array }
  deriving Show

--------------------------------------------------------------------------------

-- | Standard representation of field elements in Fp
newtype StdFp
  = StdFp { fromStdFp :: Integer }
  deriving (Eq,Show)

-- | Standard representation of field elements in Fr
newtype StdFr
  = StdFr { fromStdFr :: Integer }
  deriving (Eq,Show)

-- | Montgomery representation of field elements in Fp. For example for a 254-bit
-- prime field, it would be @(2^256 * x) `mod` p@
newtype MontgomeryFp 
  = MontgomeryFp { fromMontgomeryFp :: Integer }
  deriving (Eq,Show)

-- | Montgomery representation of field elements in Fr. For example for a 254-bit
-- prime field, it would be @(2^256 * x) `mod` p@
newtype MontgomeryFr 
  = MontgomeryFr { fromMontgomeryFr :: Integer }
  deriving (Eq,Show)

-- | Doubly Montgomery-encoded representation of field elements in Fr. 
-- This only exists because a bug in @snarkjs@... For example for a 254-bit
-- prime field, it would be @(2^512 * x) `mod` p@
newtype DoubleMontgomeryFr 
  = DoubleMontgomeryFr { fromDoubleMontgomeryFr :: Integer }
  deriving (Eq,Show)

--------------------------------------------------------------------------------

data G1 a = G1 !a !a         deriving (Eq,Show,Functor)
data G2 a = G2 !(a,a) !(a,a) deriving (Eq,Show,Functor)

readG1Array :: G1Array -> [G1 MontgomeryFp]
readG1Array (G1Array farr) = toG1 $ readForeignArrayAsList MontgomeryFp farr where
  toG1 []         = []
  toG1 (x:y:rest) = G1 x y : toG1 rest
  toG1 _          = error "readG1Array: should not happen"

readG2Array :: G2Array -> [G2 MontgomeryFp]
readG2Array (G2Array farr) = toG2 $ readForeignArrayAsList MontgomeryFp farr where
  toG2 []                 = []
  toG2 (x1:x2:y1:y2:rest) = G2 (x1,x2) (y1,y2) : toG2 rest
  toG2 _                  = error "readG2Array: should not happen"

--------------------------------------------------------------------------------

wordPowArray :: Array Int Integer
wordPowArray = listArray (0,16) [ 2^(64*i) | i<-[0..16] ]

convertToMontgomeryFp :: FieldConfig -> StdFp -> MontgomeryFp
convertToMontgomeryFp (FieldConfig elsiz (WordSize wsiz) prime) (StdFp a) = MontgomeryFp $ Math.mul prime a (wordPowArray!wsiz)

convertToMontgomeryFr :: FieldConfig -> StdFr -> MontgomeryFr
convertToMontgomeryFr (FieldConfig elsiz (WordSize wsiz) prime) (StdFr a) = MontgomeryFr $ Math.mul prime a (wordPowArray!wsiz)

convertFromMontgomeryFp :: FieldConfig -> MontgomeryFp -> StdFp
convertFromMontgomeryFp (FieldConfig elsiz (WordSize wsiz) prime) (MontgomeryFp a) = StdFp $ Math.mul prime a (Math.inv prime $ wordPowArray!wsiz)

convertFromMontgomeryFr :: FieldConfig -> MontgomeryFr -> StdFr
convertFromMontgomeryFr (FieldConfig elsiz (WordSize wsiz) prime) (MontgomeryFr a) = StdFr $ Math.mul prime a (Math.inv prime $ wordPowArray!wsiz)

convertFromDoubleMontgomeryFr :: FieldConfig -> DoubleMontgomeryFr -> StdFr
convertFromDoubleMontgomeryFr (FieldConfig elsiz (WordSize wsiz) prime) (DoubleMontgomeryFr a) = StdFr $ Math.mul prime a (Math.inv prime $ wordPowArray!(2*wsiz))

--------------------------------------------------------------------------------
