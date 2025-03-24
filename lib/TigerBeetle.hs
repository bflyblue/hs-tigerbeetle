{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module TigerBeetle (
  C.Client (..),
  I.Account (..),
  I.AccountFilter (..),
  I.InitStatus (..),
  I.ClientStatus (..),
  I.Operation (..),
  I.Packet (..),
  I.Transfer (..),
  ClusterId,
  AccountId,
  TransferId,
  LedgerId,
  newClient,
  destroyClient,
  withClient,
  createAccounts,
  lookupAccounts,
  getAccountBalances,
  getAccountTransfers,
  createTransfers,
) where

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Word (Word64)
import TigerBeetle.Identifier
import TigerBeetle.Internal.C qualified as C
import TigerBeetle.Internal.Client qualified as I
import TigerBeetle.Internal.Packet qualified as P

data TigerBeetleError
  = TBInitError I.InitStatus
  | TBClientError I.ClientStatus
  | TBPacketError I.PacketStatus
  deriving (Show, Eq, Ord)

instance Exception TigerBeetleError

data Cluster

type ClusterId = Id128 Cluster
type AccountId = Id128 I.Account
type TransferId = Id128 I.Transfer
type LedgerId = Id32 I.Ledger

newClient :: ClusterId -> ByteString -> IO C.Client
newClient (Id128 clusterId) address = do
  result <- C.clientInit clusterId address
  case result of
    Left status -> throwIO (TBInitError status)
    Right client -> pure client

destroyClient :: C.Client -> IO ()
destroyClient = C.clientDeinit

withClient :: Id128 Cluster -> ByteString -> (C.Client -> IO a) -> IO a
withClient clusterId address =
  bracket
    (newClient clusterId address)
    destroyClient

fromCallback :: (P.PacketData a) => C.Client -> IO (Either I.PacketStatus (Word64, a))
fromCallback client = do
  (timestamp, msg) <- C.waitCallback client
  case msg of
    Left status -> throwIO (TBPacketError status)
    Right somePacketData -> do
      a <- P.castSomePacketData somePacketData
      pure (Right (timestamp, a))

sendRequest :: C.Client -> I.Packet -> IO I.ClientStatus
sendRequest client packet = do
  C.sendRequest client packet

createAccounts :: C.Client -> [I.Account] -> IO [I.CreateAccountsResult]
createAccounts client accounts = do
  packet <- P.newPacketData I.OPERATION_CREATE_ACCOUNTS accounts
  clientStatus <- sendRequest client packet
  unless (clientStatus == I.CLIENT_OK) $ throwIO (TBClientError clientStatus)
  cb <- fromCallback client
  case cb of
    Left status -> throwIO (TBPacketError status)
    Right (_time, results) -> pure results

lookupAccounts :: C.Client -> [AccountId] -> IO [I.Account]
lookupAccounts client accountIds = do
  packet <- P.newPacketData I.OPERATION_LOOKUP_ACCOUNTS accountIds
  clientStatus <- sendRequest client packet
  unless (clientStatus == I.CLIENT_OK) $ throwIO (TBClientError clientStatus)
  cb <- fromCallback client
  case cb of
    Left status -> throwIO (TBPacketError status)
    Right (_time, results) -> pure results

getAccountBalances :: C.Client -> I.AccountFilter -> IO [I.AccountBalance]
getAccountBalances client accountFilter = do
  packet <- P.newPacketData I.OPERATION_GET_ACCOUNT_BALANCES accountFilter
  clientStatus <- sendRequest client packet
  unless (clientStatus == I.CLIENT_OK) $ throwIO (TBClientError clientStatus)
  cb <- fromCallback client
  case cb of
    Left status -> throwIO (TBPacketError status)
    Right (_time, results) -> pure results

getAccountTransfers :: C.Client -> I.AccountFilter -> IO [I.Transfer]
getAccountTransfers client accountFilter = do
  packet <- P.newPacketData I.OPERATION_GET_ACCOUNT_TRANSFERS accountFilter
  clientStatus <- sendRequest client packet
  unless (clientStatus == I.CLIENT_OK) $ throwIO (TBClientError clientStatus)
  cb <- fromCallback client
  case cb of
    Left status -> throwIO (TBPacketError status)
    Right (_time, results) -> pure results

createTransfers :: C.Client -> [I.Transfer] -> IO [I.CreateTransfersResult]
createTransfers client transfers = do
  packet <- P.newPacketData I.OPERATION_CREATE_TRANSFERS transfers
  clientStatus <- sendRequest client packet
  unless (clientStatus == I.CLIENT_OK) $ throwIO (TBClientError clientStatus)
  cb <- fromCallback client
  case cb of
    Left status -> throwIO (TBPacketError status)
    Right (_time, results) -> pure results