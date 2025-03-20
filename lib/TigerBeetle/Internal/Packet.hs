{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module TigerBeetle.Internal.Packet (
  PacketData (..),
  SomePacketData (..),
  castSomePacketData,
  newPacketData,
  freePacketData,
) where

import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as UBS
import Data.Coerce (coerce)
import Foreign
import TigerBeetle.Internal.Client qualified as I

class PacketData a where
  toPacketData :: a -> IO (Ptr (), Word32)
  fromPacketData :: Ptr () -> Word32 -> IO a

instance {-# OVERLAPPABLE #-} (Storable a) => PacketData a where
  toPacketData :: a -> IO (Ptr (), Word32)
  toPacketData x = do
    let size = sizeOf @a undefined
    ptr <- malloc
    poke ptr x
    pure (castPtr ptr, fromIntegral size)

  fromPacketData :: Ptr () -> Word32 -> IO a
  fromPacketData ptr _size =
    peek (castPtr ptr)

instance {-# OVERLAPPING #-} (PacketData a, Storable a) => PacketData [a] where
  toPacketData :: [a] -> IO (Ptr (), Word32)
  toPacketData xs = do
    let len = length xs
        size = len * sizeOf @a undefined
    ptr <- mallocArray len
    pokeArray ptr xs
    pure (castPtr ptr, fromIntegral size)

  fromPacketData :: Ptr () -> Word32 -> IO [a]
  fromPacketData ptr size = do
    let len = fromIntegral size `div` sizeOf @a undefined
    peekArray len (castPtr ptr)

instance {-# OVERLAPPING #-} PacketData () where
  toPacketData :: () -> IO (Ptr (), Word32)
  toPacketData () = do
    pure (nullPtr, 0)

  fromPacketData :: Ptr () -> Word32 -> IO ()
  fromPacketData _ptr _size =
    pure ()

newtype SomePacketData = SomePacketData BS.ByteString
  deriving (Show, Eq, Ord)

castSomePacketData :: (PacketData a) => SomePacketData -> IO a
castSomePacketData (SomePacketData bs) =
  UBS.unsafeUseAsCStringLen bs $
    \(ptr, size) -> fromPacketData (castPtr ptr) (fromIntegral size)

newPacketData :: (PacketData a) => I.Operation -> a -> IO I.Packet
newPacketData op d = do
  (dataPtr, dataSize) <- toPacketData d
  pure
    I.Packet
      { I.user_data = nullPtr
      , I.data_ptr = dataPtr
      , I.data_size = dataSize
      , I.user_tag = 0
      , I.operation = coerce op
      , I.status = coerce I.PACKET_OK
      }

freePacketData :: I.Packet -> IO ()
freePacketData I.Packet{I.data_ptr} =
  when (data_ptr /= nullPtr) $ free data_ptr
