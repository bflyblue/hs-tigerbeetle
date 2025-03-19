{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module TigerBeetle (
  C.Client (..),
  I.Account (..),
  I.InitStatus (..),
  I.ClientStatus (..),
  I.Operation (..),
  I.Packet (..),
  newClient,
  destroyClient,
  withClient,
  createAccounts,
) where

import Control.Concurrent (takeMVar)
import Control.Exception (Exception, bracket, throwIO)
import Data.ByteString (ByteString)
import TigerBeetle.Internal.C qualified as C
import TigerBeetle.Internal.Client qualified as I
import TigerBeetle.Internal.Packet qualified as P

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

createAccounts :: C.Client -> [I.Account] -> IO ()
createAccounts client accounts = do
  packet <- P.newPacketData I.OPERATION_CREATE_ACCOUNTS accounts
  status <- sendRequest client packet
  case status of
    I.CLIENT_OK -> waitCallback
    I.CLIENT_INVALID -> throwIO (TBClientError status)
 where
  waitCallback = takeMVar (C.cMailbox client)
