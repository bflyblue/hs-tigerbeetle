{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module TigerBeetle.Internal.C where

import Control.Exception (assert)
import Data.ByteString
import Data.WideWord
import Foreign
import Foreign.C
import GHC.ByteOrder
import qualified Language.C.Inline as C
import qualified TigerBeetle.Internal.Client as I

C.context (C.baseCtx <> C.bsCtx <> C.funCtx <> I.tigerbeetleCtx)
C.include "<tb_client.h>"

newtype Callback a = Callback (CUIntPtr -> Ptr I.Packet -> Word64 -> Ptr Word8 -> Word32 -> IO ())

mkCallback :: (a -> Ptr I.Packet -> Word64 -> Ptr Word8 -> Word32 -> IO ()) -> Callback a
mkCallback callback = Callback wrapped
 where
  wrapped ctx packet timestamp dat size = do
    ctx' <- deRefCUIntPtr ctx
    callback ctx' packet timestamp dat size

word128le :: Word128 -> Word128
word128le = case targetByteOrder of
  LittleEndian -> id
  BigEndian -> byteSwapWord128

clientInit :: Word128 -> ByteString -> a -> Callback a -> IO (Either I.InitStatus I.Client)
clientInit clusterId address ctx (Callback callback) = do
  stableCtx <- stablePtrCUIntPtr ctx
  clientInit' clusterId address stableCtx callback

clientInit' :: Word128 -> ByteString -> CUIntPtr -> (CUIntPtr -> Ptr I.Packet -> Word64 -> Ptr Word8 -> Word32 -> IO ()) -> IO (Either I.InitStatus I.Client)
clientInit' clusterId address ctx callback = do
  client <- calloc @I.TBClient
  with (word128le clusterId) $ \clusterIdPtr -> do
    let clusterIdPtr' = castPtr clusterIdPtr
    initStatus <- [C.exp| TB_INIT_STATUS { tb_client_init($(tb_client_t * client), $(uint8_t * clusterIdPtr'), $bs-ptr:address, $bs-len:address, $(uintptr_t ctx), (void (*)(uintptr_t, tb_packet_t *, uint64_t, const uint8_t *, uint32_t))$fun:(void (*callback)(uintptr_t, tb_packet_t *, uint64_t, uint8_t *, uint32_t))) } |]
    case initStatus of
      I.INIT_SUCCESS -> pure (Right client)
      s -> do
        free client
        pure (Left s)

stablePtrCUIntPtr :: a -> IO CUIntPtr
stablePtrCUIntPtr x =
  assert (sizeOf (undefined :: CUIntPtr) == sizeOf (undefined :: StablePtr a)) $ do
    stablePtr <- newStablePtr x
    alloca $ \(ptr :: Ptr CUIntPtr) -> do
      poke (castPtr ptr) stablePtr
      peek (castPtr ptr)

deRefCUIntPtr :: CUIntPtr -> IO a
deRefCUIntPtr x =
  assert (sizeOf (undefined :: CUIntPtr) == sizeOf (undefined :: StablePtr a)) $ do
    alloca $ \(ptr :: Ptr (StablePtr a)) -> do
      poke (castPtr ptr) x
      stablePtr <- peek (castPtr ptr)
      deRefStablePtr stablePtr

clientDeinit :: I.Client -> IO ()
clientDeinit client = do
  [C.block| void { tb_client_deinit($(tb_client_t * client)); } |]
  free client

ptrToCUIntPtr :: Ptr a -> CUIntPtr
ptrToCUIntPtr ptr =
  case toIntegralSized (ptrToWordPtr ptr) of
    Just cuintptr -> cuintptr
    Nothing -> error "Pointer conversion failed due to size mismatch"

sendRequest :: I.Client -> I.Packet -> IO I.ClientStatus
sendRequest client packet = do
  packetPtr <- newStablePtr packet
  sendRequest' client (castPtr (castStablePtrToPtr packetPtr))

sendRequest' :: I.Client -> Ptr I.Packet -> IO I.ClientStatus
sendRequest' client packet = do
  [C.block| TB_CLIENT_STATUS { tb_client_submit($(tb_client_t * client), $(tb_packet_t * packet)); } |]
