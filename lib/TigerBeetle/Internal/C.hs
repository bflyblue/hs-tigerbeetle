{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module TigerBeetle.Internal.C where

import Control.Concurrent
import Data.ByteString
import Data.WideWord
import Foreign
import Foreign.C
import GHC.ByteOrder
import Language.C.Inline qualified as C
import TigerBeetle.Internal.Client qualified as Client
import TigerBeetle.Internal.Context qualified as Ctx
import TigerBeetle.Internal.Packet qualified as P

C.context (C.baseCtx <> C.bsCtx <> C.funCtx <> Ctx.tigerbeetleCtx)
C.include "<tb_client.h>"

data Client = Client
  { cClient :: Client.Client
  -- ^ The underlying C client
  , cCallbackFunPtr :: FunPtr (CUIntPtr -> Ptr Client.Packet -> Word64 -> Ptr Word8 -> Word32 -> IO ())
  -- ^ Pointer to the callback function
  , cMailbox :: MVar ()
  -- ^ The mailbox to communicate with the callback
  , cPacketPtr :: Ptr Client.Packet
  -- ^ Pointer to memory allocated to store the packet
  }

clientInit :: Word128 -> ByteString -> IO (Either Client.InitStatus Client)
clientInit clusterId address = do
  -- Allocate space to store the opaque client handle
  cClient <- calloc @Client.TBClient

  -- For now, we don't need a context, so we can pass nullPtr
  let ctx = ptrToCUIntPtr nullPtr

  -- Create a mailbox to communicate with the callback
  cMailbox <- newEmptyMVar

  -- Create a callback function that puts a message in the mailbox
  cCallbackFunPtr <- mkCallback (callback cMailbox)

  -- Allocate space to store the packet
  cPacketPtr <- malloc @Client.Packet

  -- Initialize the client
  initStatus <- with (word128le clusterId) $ \clusterIdPtr -> do
    let clusterIdPtr' = castPtr clusterIdPtr
    [C.exp| TB_INIT_STATUS {
      tb_client_init(
        $(tb_client_t * cClient),
        $(uint8_t * clusterIdPtr'),
        $bs-ptr:address,
        $bs-len:address,
        $(uintptr_t ctx),
        $(void (*cCallbackFunPtr)(uintptr_t, tb_packet_t *, uint64_t, const uint8_t *, uint32_t))
      )
    } |]

  case initStatus of
    Client.INIT_SUCCESS ->
      -- If the client was initialized successfully, return it
      pure (Right Client{cClient, cCallbackFunPtr, cMailbox, cPacketPtr})
    s -> do
      -- Otherwise, free the client and return the error
      free cClient
      freeHaskellFunPtr cCallbackFunPtr
      pure (Left s)

callback :: MVar () -> CUIntPtr -> Ptr Client.Packet -> Word64 -> Ptr Word8 -> Word32 -> IO ()
callback mbox _ctxIntPtr packetPtr timestamp dataPtr size = do
  packet <- peek packetPtr
  print (dataPtr, size)
  putMVar mbox ()
  P.freePacketData packet

clientDeinit :: Client -> IO ()
clientDeinit Client{cClient, cPacketPtr} = do
  free cPacketPtr
  [C.exp| void { tb_client_deinit($(tb_client_t * cClient)) } |]
  free cClient

clientCompletionContext :: Client -> IO (Either Client.ClientStatus CUIntPtr)
clientCompletionContext Client{cClient} = do
  alloca $ \ctxPtrPtr -> do
    clientStatus <- [C.exp| TB_CLIENT_STATUS { tb_client_completion_context($(tb_client_t * cClient), $(uintptr_t * ctxPtrPtr)) } |]
    case clientStatus of
      Client.CLIENT_OK ->
        Right <$> peek ctxPtrPtr
      s -> pure (Left s)

sendRequest :: Client -> Client.Packet -> IO Client.ClientStatus
sendRequest Client{cClient, cPacketPtr} packet = do
  poke cPacketPtr packet
  [C.block| TB_CLIENT_STATUS { tb_client_submit($(tb_client_t * cClient), $(tb_packet_t * cPacketPtr)); } |]

-- * Callbacks

type Callback = CUIntPtr -> Ptr Client.Packet -> Word64 -> Ptr Word8 -> Word32 -> IO ()

foreign import ccall "wrapper" mkCallback :: Callback -> IO (FunPtr Callback)

-- * Utility functions

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

word128le :: Word128 -> Word128
word128le = case targetByteOrder of
  LittleEndian -> id
  BigEndian -> byteSwapWord128
