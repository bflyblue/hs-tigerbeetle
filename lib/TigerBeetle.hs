module TigerBeetle where

import Control.Concurrent
import Control.Exception
import Data.ByteString
import Data.Coerce (coerce)
import Foreign
import qualified TigerBeetle.Internal.C as C
import qualified TigerBeetle.Internal.Client as I

data TigerBeetleError
  = TBInitError I.InitStatus
  | TBClientError I.ClientStatus
  deriving (Show, Eq, Ord)

instance Exception TigerBeetleError

newClient :: Integer -> ByteString -> IO C.Client
newClient clusterId address = do
  result <- C.clientInit (fromInteger clusterId) address
  case result of
    Left status -> throwIO (TBInitError status)
    Right client -> pure client

destroyClient :: C.Client -> IO ()
destroyClient = C.clientDeinit

withClient :: Integer -> ByteString -> (C.Client -> IO a) -> IO a
withClient clusterId address =
  bracket
    (newClient clusterId address)
    destroyClient

sendRequest :: C.Client -> I.Packet -> IO I.ClientStatus
sendRequest client packet = do
  C.sendRequest client packet

pulse :: C.Client -> IO ()
pulse client = do
  datPtr <- mallocBytes 1024
  userPtr <- mallocBytes 1024

  let packet =
        I.Packet
          { I.user_data = userPtr
          , I.data_ptr = datPtr
          , I.data_size = 1024
          , I.user_tag = 0
          , I.operation = coerce I.OPERATION_PULSE
          , I.status = coerce I.PACKET_OK
          }
  status <- sendRequest client packet
  case status of
    I.CLIENT_OK -> waitCallback
    _ -> throwIO (TBClientError status)
 where
  waitCallback = takeMVar (C.cMailbox client)
