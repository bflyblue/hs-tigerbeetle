module TigerBeetle.Internal.Amount where

import Data.Bits
import Data.Primitive.Types
import Data.WideWord (Word128)
import Foreign.Storable

newtype Amount = Amount Word128 deriving (Eq, Ord, Show, Read, Num, Real, Storable, Prim, Bits)