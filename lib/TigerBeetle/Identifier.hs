module TigerBeetle.Identifier where

import Data.Bits (Bits)
import Data.Primitive (Prim)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.WideWord (Word128 (Word128), showHexWord128, showHexWord64)
import Data.Word (Word32, Word64)
import Foreign.Storable (Storable)

newtype Id128 a = Id128 Word128 deriving (Eq, Ord, Show, Read, Num, Enum, Real, Integral, Storable, Prim, Bits)

fromWord128 :: Word128 -> Id128 a
fromWord128 = Id128

toWord128 :: Id128 a -> Word128
toWord128 (Id128 w) = w

showHexId128 :: Id128 a -> String
showHexId128 (Id128 w) = showHexWord128 w

fromUUID :: UUID -> Id128 a
fromUUID uuid =
  let (hi, lo) = UUID.toWords64 uuid
   in Id128 (Word128 hi lo)

toUUID :: Id128 a -> UUID
toUUID (Id128 (Word128 hi lo)) = UUID.fromWords64 hi lo

newtype Id64 a = Id64 Word64 deriving (Eq, Ord, Show, Read, Num, Enum, Real, Integral, Storable, Prim, Bits)

fromWord64 :: Word64 -> Id64 a
fromWord64 = Id64

toWord64 :: Id64 a -> Word64
toWord64 (Id64 w) = w

showHexId64 :: Id64 a -> String
showHexId64 (Id64 w) = showHexWord64 w

newtype Id32 a = Id32 Word32 deriving (Eq, Ord, Show, Read, Num, Enum, Real, Integral, Storable, Prim, Bits)

fromWord32 :: Word32 -> Id32 a
fromWord32 = Id32

toWord32 :: Id32 a -> Word32
toWord32 (Id32 w) = w