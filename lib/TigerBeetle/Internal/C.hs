{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module TigerBeetle.Internal.C where

import Control.Concurrent
import Data.ByteString
import Data.WideWord
import Foreign
import Foreign.C
import GHC.ByteOrder
import qualified Language.C.Inline as C
import qualified TigerBeetle.Internal.Client as I

C.context (C.baseCtx <> C.bsCtx <> C.funCtx <> I.tigerbeetleCtx)
C.include "<tb_client.h>"

data Callback a = Callback
  { cbFunction :: a -> Ptr I.Packet -> Word64 -> Ptr Word8 -> Word32 -> IO ()
  , cbPtr :: FunPtr (CUIntPtr -> Ptr I.Packet -> Word64 -> Ptr Word8 -> Word32 -> IO ())
  }

data Client = Client
  { cClient :: I.Client
  , cCallback :: Callback (MVar ())
  , cPacketPtr :: Ptr I.Packet
  , cMailbox :: MVar ()
  }

mkCallback :: (a -> Ptr I.Packet -> Word64 -> Ptr Word8 -> Word32 -> IO ()) -> IO (Callback a)
mkCallback callback = do
  callbackPtr <- [C.exp| void (*callback)(uintptr_t, tb_packet_t *, uint64_t, uint8_t *, uint32_t) { $fun-alloc:(void (*wrapped)(uintptr_t, tb_packet_t *, uint64_t, uint8_t *, uint32_t)) } |]
  pure Callback{cbFunction = callback, cbPtr = callbackPtr}
 where
  wrapped :: CUIntPtr -> Ptr I.Packet -> Word64 -> Ptr Word8 -> Word32 -> IO ()
  wrapped ctxPtr packet timestamp dat size = do
    putStrLn "wrapped"
    ctx <- deRefStablePtr (castPtrToStablePtr $ cuintPtrToPtr ctxPtr)
    callback ctx packet timestamp dat size

word128le :: Word128 -> Word128
word128le = case targetByteOrder of
  LittleEndian -> id
  BigEndian -> byteSwapWord128

clientInit :: Word128 -> ByteString -> Callback (MVar ()) -> IO (Either I.InitStatus Client)
clientInit clusterId address callback = do
  mbox <- newEmptyMVar
  ctxPtr <- newStablePtr mbox
  result <- clientInit' clusterId address (ptrToCUIntPtr $ castStablePtrToPtr ctxPtr) (cbPtr callback)
  case result of
    Left status -> do
      freeStablePtr ctxPtr
      pure (Left status)
    Right client -> do
      packetPtr <- malloc
      pure (Right Client{cClient = client, cCallback = callback, cPacketPtr = packetPtr, cMailbox = mbox})

clientInit' :: Word128 -> ByteString -> CUIntPtr -> FunPtr (CUIntPtr -> Ptr I.Packet -> Word64 -> Ptr Word8 -> Word32 -> IO ()) -> IO (Either I.InitStatus I.Client)
clientInit' clusterId address ctx callbackPtr = do
  client <- calloc @I.TBClient
  with (word128le clusterId) $ \clusterIdPtr -> do
    let clusterIdPtr' = castPtr clusterIdPtr
    initStatus <-
      [C.exp| TB_INIT_STATUS { tb_client_init($(tb_client_t * client), $(uint8_t * clusterIdPtr'), $bs-ptr:address, $bs-len:address, $(uintptr_t ctx), $(void (*callbackPtr)(uintptr_t, tb_packet_t *, uint64_t, const uint8_t *, uint32_t))) } |]
    case initStatus of
      I.INIT_SUCCESS -> pure (Right client)
      s -> do
        free client
        freeHaskellFunPtr callbackPtr
        pure (Left s)

clientDeinit :: I.Client -> IO ()
clientDeinit client = do
  alloca $ \ctxPtrPtr -> do
    clientStatus <- [C.exp| TB_CLIENT_STATUS { tb_client_completion_context($(tb_client_t * client), $(uintptr_t * ctxPtrPtr)) } |]
    ctxPtr <- peek ctxPtrPtr
    freeStablePtr (castPtrToStablePtr $ cuintPtrToPtr ctxPtr)
  [C.exp| void { tb_client_deinit($(tb_client_t * client)) } |]
  free client

ptrToCUIntPtr :: Ptr a -> CUIntPtr
ptrToCUIntPtr ptr =
  case toIntegralSized (ptrToWordPtr ptr) of
    Just cuintptr -> cuintptr
    Nothing -> error "Pointer conversion failed due to size mismatch"

cuintPtrToPtr :: CUIntPtr -> Ptr a
cuintPtrToPtr cuintptr =
  case toIntegralSized cuintptr of
    Just ptr -> wordPtrToPtr ptr
    Nothing -> error "Pointer conversion failed due to size mismatch"

sendRequest :: I.Client -> Ptr I.Packet -> IO I.ClientStatus
sendRequest client packet = do
  [C.block| TB_CLIENT_STATUS { tb_client_submit($(tb_client_t * client), $(tb_packet_t * packet)); } |]
