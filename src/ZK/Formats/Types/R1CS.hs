
-- | Types related to R1CS 

--------------------------------------------------------------------------------

{-# LANGUAGE StrictData, GeneralizedNewtypeDeriving #-}
module ZK.Formats.Types.R1CS where

--------------------------------------------------------------------------------

import Data.Array
import Data.Word

import ZK.Formats.Types.Etc
import ZK.Formats.Primes

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
  { _r1csFieldConfig :: FieldConfig            -- ^ prime field configuration
  , _witnessCfg      :: WitnessConfig          -- ^ witness configuration (public and private inputs)
  , _constraints     :: [Constraint]           -- ^ the list of R1CS constraints
  , _wireToLabelId   :: WireToLabelId          -- ^ mapping of wire indices to label ids
  , _customGates     :: Maybe CustomGates      -- ^ custom gates 
  }
  deriving Show

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

fromWireToLabelId :: WireToLabelId -> Array WireIdx Word64
fromWireToLabelId (WireToLabelId arr) = arr

--------------------------------------------------------------------------------
-- * Custom gates \/ templates 

-- | Custom gates (or templates) in the circuit
data CustomGates = CustomGates
  { _customGateArray :: Array Int CustomGateDef
  , _customGateApps  :: [CustomGateApp]
  }
  deriving Show

-- | Declaration of custom gates
data CustomGateDef = CustomGateDef 
  { _customGateName :: String        -- ^ name of the custom template
  , _customGateArgs :: [Integer]     -- ^ template arguments of this particular instantiation of the custom template
  }
  deriving Show

-- | Application of custom gates
data CustomGateApp = CustomGateApp
  { _customGateIndex :: Int          -- ^ index of the custom template instantiation (in the above array)
  , _customGateWires :: [WireIdx]    -- ^ wires connecting this gate to the circuit (same order as the in the circom source file)
  }
  deriving Show

--------------------------------------------------------------------------------
