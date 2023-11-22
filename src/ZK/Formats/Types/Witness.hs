
-- | Types realted to the witness

--------------------------------------------------------------------------------

{-# LANGUAGE StrictData #-}
module ZK.Formats.Types.Witness where

--------------------------------------------------------------------------------

import Data.Array

import ZK.Formats.Types.Etc
import ZK.Formats.ForeignArray

--------------------------------------------------------------------------------

data Witness = Witness 
  { _witnessFieldConfig   :: FieldConfig        -- ^ prime field configuration
  , _numberOfWitnessVars  :: Int                -- ^ number of witness variables (including the special variable "1")
  , _witnessData          :: StdFrArray         -- ^ raw data of the witness variables
  }
  deriving Show

witnessArray :: Witness -> Array Int Integer
witnessArray wtns = readForeignArray id (fromStdFrArray $ _witnessData wtns)

witnessList :: Witness -> [Integer]
witnessList = elems . witnessArray

--------------------------------------------------------------------------------
