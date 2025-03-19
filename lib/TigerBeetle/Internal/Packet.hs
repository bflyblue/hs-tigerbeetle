module TigerBeetle.Internal.Packet (
  PacketData (..),
  newPacketData,
  freePacketData,
) where

import Control.Monad (when)
import Data.Coerce (coerce)
import Foreign
import TigerBeetle.Internal.Client qualified as I

class PacketData a where
  toPacketData :: a -> IO (Ptr (), Word32)
  fromPacketData :: Ptr () -> Word32 -> IO a

instance PacketData () where
  toPacketData :: () -> IO (Ptr (), Word32)
  toPacketData () = do
    pure (nullPtr, 0)

  fromPacketData :: Ptr () -> Word32 -> IO ()
  fromPacketData _ptr _size =
    pure ()

instance PacketData I.Account where
  toPacketData :: I.Account -> IO (Ptr (), Word32)
  toPacketData x = do
    ptr <- malloc
    poke ptr x
    pure (castPtr ptr, fromIntegral (sizeOf x))

  fromPacketData :: Ptr () -> Word32 -> IO I.Account
  fromPacketData ptr _size =
    peek (castPtr ptr)

instance (PacketData a, Storable a) => PacketData [a] where
  toPacketData :: [a] -> IO (Ptr (), Word32)
  toPacketData xs = do
    let
      len = length xs
      size = len * sizeOf @a undefined
    ptr <- mallocArray len
    pokeArray ptr xs
    pure (castPtr ptr, fromIntegral size)

  fromPacketData :: Ptr () -> Word32 -> IO [a]
  fromPacketData ptr size = do
    let len = fromIntegral size `div` sizeOf @a undefined
    peekArray len (castPtr ptr)

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
