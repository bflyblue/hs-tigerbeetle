module TigerBeetle where

import Control.Concurrent
import Control.Exception
import Data.ByteString
import Foreign
import qualified TigerBeetle.Internal.C as C
import qualified TigerBeetle.Internal.Client as I
import Data.Coerce (coerce)

data Client = Client
  { cClient :: I.Client
  , cMailbox :: MVar ()
  }

data TigerBeetleError
  = TBInitError I.InitStatus
  | TBClientError I.ClientStatus
  deriving (Show, Eq, Ord)

instance Exception TigerBeetleError

newtype RequestContext = RequestContext
  { rMailbox :: MVar ()
  }

newClient :: Integer -> ByteString -> IO Client
newClient clusterId address = do
  mbox <- newMVar ()
  let rctx = RequestContext mbox
  result <- C.clientInit (fromInteger clusterId) address rctx (C.mkCallback onCompletion)
  case result of
    Left status -> throwIO (TBInitError status)
    Right client -> pure Client { cClient = client, cMailbox = mbox }

destroyClient :: Client -> IO ()
destroyClient client = do
  C.clientDeinit (cClient client)

withClient :: Integer -> ByteString -> (Client -> IO a) -> IO a
withClient clusterId address =
  bracket
    (newClient clusterId address)
    destroyClient

onCompletion :: RequestContext -> Ptr I.Packet -> Word64 -> Ptr Word8 -> Word32 -> IO ()
onCompletion rctx packetPtr timestamp dat size = do
  putMVar (rMailbox rctx) ()

sendRequest :: Client -> I.Packet -> IO I.ClientStatus
sendRequest client = C.sendRequest (cClient client)

pulse :: Client -> IO ()
pulse client = do
  let packet = I.Packet {
    I.user_data = nullPtr,
    I.data_ptr = nullPtr,
    I.data_size = 0,
    I.user_tag = 0,
    I.operation = coerce I.OPERATION_PULSE,
    I.status = coerce I.PACKET_OK
  }
  status <- sendRequest client packet
  case status of
    I.CLIENT_OK -> waitCallback
    _ -> throwIO (TBClientError status)
 where
  waitCallback = do
    takeMVar (cMailbox client)
