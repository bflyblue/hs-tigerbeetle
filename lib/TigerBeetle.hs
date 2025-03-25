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
  I.AccountFlags (..),
  I.CreateAccountResult (..),
  I.CreateAccountsResult (..),
  I.CreateTransfersResult (..),
  I.TransferFlags (..),
  Amount (..),
  Timestamp (..),
  I.Code (..),
  ClusterId,
  AccountId,
  TransferId,
  LedgerId,
  newClient,
  destroyClient,
  withClient,
  newEchoClient,
  withEchoClient,
  sendRequest,
  echoRequest,

  -- * TigerBeetle Requests
  createAccounts,
  createTransfers,
  lookupAccounts,
  lookupTransfers,
  getAccountTransfers,
  getAccountBalances,
  queryAccounts,
  queryTransfers,

  -- * Echo Requests
  echoCreateAccounts,
  echoCreateTransfers,
  echoLookupAccounts,
  echoLookupTransfers,
  echoGetAccountTransfers,
  echoGetAccountBalances,
  echoQueryAccounts,
  echoQueryTransfers,
) where

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Word (Word64)
import TigerBeetle.Identifier
import TigerBeetle.Internal.Amount
import TigerBeetle.Internal.C qualified as C
import TigerBeetle.Internal.Client qualified as I
import TigerBeetle.Internal.Packet qualified as P
import TigerBeetle.Internal.Timestamp

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

newEchoClient :: ClusterId -> ByteString -> IO C.Client
newEchoClient (Id128 clusterId) address = do
  result <- C.clientInitEcho clusterId address
  case result of
    Left status -> throwIO (TBInitError status)
    Right client -> pure client

withEchoClient :: ClusterId -> ByteString -> (C.Client -> IO a) -> IO a
withEchoClient clusterId address =
  bracket
    (newEchoClient clusterId address)
    destroyClient

fromCallback :: (P.PacketData a) => C.Client -> IO (Either I.PacketStatus (Word64, a))
fromCallback client = do
  (timestamp, msg) <- C.waitCallback client
  case msg of
    Left status -> throwIO (TBPacketError status)
    Right somePacketData -> do
      a <- P.castSomePacketData somePacketData
      pure (Right (timestamp, a))

sendRequest :: (P.PacketData a, P.PacketData b) => C.Client -> I.Operation -> a -> IO b
sendRequest client op body = do
  packet <- P.newPacketData op body
  clientStatus <- C.sendRequest client packet
  unless (clientStatus == I.CLIENT_OK) $ throwIO (TBClientError clientStatus)
  cb <- fromCallback client
  case cb of
    Left status -> throwIO (TBPacketError status)
    Right (_time, results) -> pure results

echoRequest :: (P.PacketData a) => C.Client -> I.Operation -> a -> IO a
echoRequest client op body = do
  packet <- P.newPacketData op body
  clientStatus <- C.sendRequest client packet
  unless (clientStatus == I.CLIENT_OK) $ throwIO (TBClientError clientStatus)
  cb <- fromCallback client
  case cb of
    Left status -> throwIO (TBPacketError status)
    Right (_time, results) -> pure results

-- * TigerBeetle Requests

{- | Create one or more Accounts.

Result
Results are listed in this section in order of descending precedence — that is,
if more than one error is applicable to the account being created, only the
result listed first is returned.

`ok`

The account was successfully created; it did not previously exist.

Note that ok is generated by the client implementation; the network protocol
does not include a result when the account was successfully created.

`linked_event_failed`

The account was not created. One or more of the accounts in the linked chain is
invalid, so the whole chain failed.

`linked_event_chain_open`

The account was not created. The Account.flags.linked flag was set on the last
event in the batch, which is not legal. (flags.linked indicates that the chain
continues to the next operation).

`imported_event_expected`

The account was not created. The Account.flags.imported was set on the first
account of the batch, but not all accounts in the batch. Batches cannot mix
imported accounts with non-imported accounts.

`imported_event_not_expected`

The account was not created. The Account.flags.imported was expected to not be
set, as it's not allowed to mix accounts with different imported flag in the
same batch. The first account determines the entire operation.

`timestamp_must_be_zero`

This result only applies when Account.flags.imported is not set.

The account was not created. The Account.timestamp is nonzero, but must be zero.
The cluster is responsible for setting this field.

The Account.timestamp can only be assigned when creating accounts with
Account.flags.imported set.

`imported_event_timestamp_out_of_range`

This result only applies when Account.flags.imported is set.

The account was not created. The Account.timestamp is out of range, but must be
a user-defined timestamp greater than 0 and less than 2^63.

`imported_event_timestamp_must_not_advance`

This result only applies when Account.flags.imported is set.

The account was not created. The user-defined Account.timestamp is greater than
the current cluster time, but it must be a past timestamp.

`reserved_field`

The account was not created. Account.reserved is nonzero, but must be zero.

`reserved_flag`

The account was not created. Account.flags.reserved is nonzero, but must be zero.

`id_must_not_be_zero`

The account was not created. Account.id is zero, which is a reserved value.

`id_must_not_be_int_max`

The account was not created. Account.id is 2^128 - 1, which is a reserved value.

`exists_with_different_flags`

An account with the same id already exists, but with different flags.

`exists_with_different_user_data_128`

An account with the same id already exists, but with different user_data_128.

`exists_with_different_user_data_64`

An account with the same id already exists, but with different user_data_64.

`exists_with_different_user_data_32`

An account with the same id already exists, but with different user_data_32.

`exists_with_different_ledger`

An account with the same id already exists, but with different ledger.

`exists_with_different_code`

An account with the same id already exists, but with different code.

`exists`

An account with the same id already exists.

With the possible exception of the following fields, the existing account is
identical to the account in the request:

 * `timestamp`
 * `debits_pending`
 * `debits_posted`
 * `credits_pending`
 * `credits_posted`

To correctly recover from application crashes, many applications should handle
exists exactly as ok.

`flags_are_mutually_exclusive`

The account was not created. An account cannot be created with the specified
combination of Account.flags.

The following flags are mutually exclusive:

 * `Account.flags.debits_must_not_exceed_credits`
 * `Account.flags.credits_must_not_exceed_debits`

`debits_pending_must_be_zero`

The account was not created. Account.debits_pending is nonzero, but must be zero.

An account's debits and credits are only modified by transfers.

`debits_posted_must_be_zero`

The account was not created. Account.debits_posted is nonzero, but must be zero.

An account's debits and credits are only modified by transfers.

`credits_pending_must_be_zero`

The account was not created. Account.credits_pending is nonzero, but must be zero.

An account's debits and credits are only modified by transfers.

`credits_posted_must_be_zero`

The account was not created. Account.credits_posted is nonzero, but must be zero.

An account's debits and credits are only modified by transfers.

`ledger_must_not_be_zero`

The account was not created. Account.ledger is zero, but must be nonzero.

`code_must_not_be_zero`

The account was not created. Account.code is zero, but must be nonzero.

`imported_event_timestamp_must_not_regress`

This result only applies when Account.flags.imported is set.

The account was not created. The user-defined Account.timestamp regressed, but
it must be greater than the last timestamp assigned to any Account in the
cluster and cannot be equal to the timestamp of any existing Transfer.
-}
createAccounts :: C.Client -> [I.Account] -> IO [I.CreateAccountsResult]
createAccounts client = sendRequest client I.OPERATION_CREATE_ACCOUNTS

echoCreateAccounts :: C.Client -> [I.Account] -> IO [I.Account]
echoCreateAccounts client = echoRequest client I.OPERATION_CREATE_ACCOUNTS

{- | Create one or more Transfers. A successfully created transfer will modify
     the amount fields of its debit and credit accounts.

Result
Results are listed in this section in order of descending precedence — that is,
if more than one error is applicable to the transfer being created, only the
result listed first is returned.

`ok`

The transfer was successfully created; did not previously exist.

Note that ok is generated by the client implementation; the network protocol
does not include a result when the transfer was successfully created.

`linked_event_failed`

The transfer was not created. One or more of the other transfers in the linked
chain is invalid, so the whole chain failed.

`linked_event_chain_open`

The transfer was not created. The Transfer.flags.linked flag was set on the last
event in the batch, which is not legal. (flags.linked indicates that the chain
continues to the next operation).

`imported_event_expected`

The transfer was not created. The Transfer.flags.imported was set on the first
transfer of the batch, but not all transfers in the batch. Batches cannot mix
imported transfers with non-imported transfers.

`imported_event_not_expected`

The transfer was not created. The Transfer.flags.imported was expected to not be
set, as it's not allowed to mix transfers with different imported flag in the
same batch. The first transfer determines the entire operation.

`timestamp_must_be_zero`

This result only applies when Account.flags.imported is not set.

The transfer was not created. The Transfer.timestamp is nonzero, but must be
zero. The cluster is responsible for setting this field.

The Transfer.timestamp can only be assigned when creating transfers with
Transfer.flags.imported set.

`imported_event_timestamp_out_of_range`

This result only applies when Transfer.flags.imported is set.

The transfer was not created. The Transfer.timestamp is out of range, but must
be a user-defined timestamp greater than 0 and less than 2^63.

`imported_event_timestamp_must_not_advance`

This result only applies when Transfer.flags.imported is set.

The transfer was not created. The user-defined Transfer.timestamp is greater
than the current cluster time, but it must be a past timestamp.

`reserved_flag`

The transfer was not created. Transfer.flags.reserved is nonzero, but must be
zero.

`id_must_not_be_zero`

The transfer was not created. Transfer.id is zero, which is a reserved value.

`id_must_not_be_int_max`

The transfer was not created. Transfer.id is 2^128 - 1, which is a reserved
value.

`exists_with_different_flags`

A transfer with the same id already exists, but with different flags.

`exists_with_different_pending_id`

A transfer with the same id already exists, but with a different pending_id.

`exists_with_different_timeout`

A transfer with the same id already exists, but with a different timeout.

`exists_with_different_debit_account_id`

A transfer with the same id already exists, but with a different debit_account_id.

`exists_with_different_credit_account_id`

A transfer with the same id already exists, but with a different credit_account_id.

`exists_with_different_amount`

A transfer with the same id already exists, but with a different amount.

If the transfer has flags.balancing_debit or flags.balancing_credit set, then
the actual amount transferred exceeds this failed transfer's amount.

`exists_with_different_user_data_128`

A transfer with the same id already exists, but with a different user_data_128.

`exists_with_different_user_data_64`

A transfer with the same id already exists, but with a different user_data_64.

`exists_with_different_user_data_32`

A transfer with the same id already exists, but with a different user_data_32.

`exists_with_different_ledger`

A transfer with the same id already exists, but with a different ledger.

`exists_with_different_code`

A transfer with the same id already exists, but with a different code.

`exists`

A transfer with the same id already exists.

If the transfer has flags.balancing_debit or flags.balancing_credit set, then
the existing transfer may have a different amount, limited to the maximum amount
of the transfer in the request.

If the transfer has flags.post_pending_transfer set, then the existing transfer
may have a different amount:

 * If the original posted amount was less than the pending amount, then the
transfer amount must be equal to the posted amount.
 * Otherwise, the transfer amount must be greater than or equal to the pending
   amount.
 * Otherwise, with the possible exception of the timestamp field, the existing
 transfer is identical to the transfer in the request. (Client release < 0.16.0)

To correctly recover from application crashes, many applications should handle
exists exactly as ok.

`id_already_failed`

The transfer was not created. A previous transfer with the same id failed due to
one of the following transient errors:

 * `debit_account_not_found`
 * `credit_account_not_found`
 * `pending_transfer_not_found`
 * `exceeds_credits`
 * `exceeds_debits`
 * `debit_account_already_closed`
 * `credit_account_already_closed`

Transient errors depend on the database state at a given point in time, and each
attempt is uniquely associated with the corresponding Transfer.id. This behavior
guarantees that retrying a transfer will not produce a different outcome (either
success or failure).

Without this mechanism, a transfer that previously failed could succeed if
retried when the underlying state changes (e.g., the target account has
sufficient credits).

**Note: The application should retry an event only if it was unable to acknowledge
the last response (e.g., due to an application restart) or because it is
correcting a previously rejected malformed request (e.g., due to an application
bug). If the application intends to submit the transfer again even after a
transient error, it must generate a new idempotency id.

The id is never checked against failed transfers, regardless of the error.
Therefore, a transfer that failed due to a transient error could succeed if
retried later. (Client release < 0.16.4)

`flags_are_mutually_exclusive`

The transfer was not created. A transfer cannot be created with the specified
combination of Transfer.flags.

Flag compatibility (✓ = compatible, ✗ = mutually exclusive):

* `flags.pending`

    * ✗ `flags.post_pending_transfer`
    * ✗ `flags.void_pending_transfer`
    * ✓ `flags.balancing_debit`
    * ✓ `flags.balancing_credit`
    * ✓ `flags.closing_debit`
    * ✓ `flags.closing_credit`
    * ✓ `flags.imported`

* `flags.post_pending_transfer`

    * ✗ `flags.pending`
    * ✗ `flags.void_pending_transfer`
    * ✗ `flags.balancing_debit`
    * ✗ `flags.balancing_credit`
    * ✗ `flags.closing_debit`
    * ✗ `flags.closing_credit`
    * ✓ `flags.imported`

* `flags.void_pending_transfer`

    * ✗ `flags.pending`
    * ✗ `flags.post_pending_transfer`
    * ✗ `flags.balancing_debit`
    * ✗ `flags.balancing_credit`
    * ✗ `flags.closing_debit`
    * ✗ `flags.closing_credit`
    * ✓ `flags.imported`

* `flags.balancing_debit`

    * ✓ `flags.pending`
    * ✗ `flags.void_pending_transfer`
    * ✗ `flags.post_pending_transfer`
    * ✓ `flags.balancing_credit`
    * ✓ `flags.closing_debit`
    * ✓ `flags.closing_credit`
    * ✓ `flags.imported`

* `flags.balancing_credit`

    * ✓ `flags.pending`
    * ✗ `flags.void_pending_transfer`
    * ✗ `flags.post_pending_transfer`
    * ✓ `flags.balancing_debit`
    * ✓ `flags.closing_debit`
    * ✓ `flags.closing_credit`
    * ✓ `flags.imported`

* `flags.closing_debit`

    * ✓ `flags.pending`
    * ✗ `flags.post_pending_transfer`
    * ✗ `flags.void_pending_transfer`
    * ✓ `flags.balancing_debit`
    * ✓ `flags.balancing_credit`
    * ✓ `flags.closing_credit`
    * ✓ `flags.imported`

* `flags.closing_credit`

    * ✓ `flags.pending`
    * ✗ `flags.post_pending_transfer`
    * ✗ `flags.void_pending_transfer`
    * ✓ `flags.balancing_debit`
    * ✓ `flags.balancing_credit`
    * ✓ `flags.closing_debit`
    * ✓ `flags.imported`

* `flags.imported`

    * ✓ `flags.pending`
    * ✓ `flags.post_pending_transfer`
    * ✓ `flags.void_pending_transfer`
    * ✓ `flags.balancing_debit`
    * ✓ `flags.balancing_credit`
    * ✓ `flags.closing_debit`
    * ✓ `flags.closing_credit`

`debit_account_id_must_not_be_zero`

The transfer was not created. Transfer.debit_account_id is zero, but must be a
valid account id.

`debit_account_id_must_not_be_int_max`

The transfer was not created. Transfer.debit_account_id is 2^128 - 1, but must
be a valid account id.

`credit_account_id_must_not_be_zero`

The transfer was not created. Transfer.credit_account_id is zero, but must be a
valid account id.

`credit_account_id_must_not_be_int_max`

The transfer was not created. Transfer.credit_account_id is 2^128 - 1, but must
be a valid account id.

`accounts_must_be_different`

The transfer was not created. Transfer.debit_account_id and
Transfer.credit_account_id must not be equal.

That is, an account cannot transfer money to itself.

`pending_id_must_be_zero`

The transfer was not created. Only post/void transfers can reference a pending
transfer.

Either:

 * Transfer.flags.post_pending_transfer must be set, or
 * Transfer.flags.void_pending_transfer must be set, or
 * Transfer.pending_id must be zero.

`pending_id_must_not_be_zero`

The transfer was not created. Transfer.flags.post_pending_transfer or
Transfer.flags.void_pending_transfer is set, but Transfer.pending_id is zero. A
posting or voiding transfer must reference a pending transfer.

`pending_id_must_not_be_int_max`

The transfer was not created. Transfer.pending_id is 2^128 - 1, which is a
reserved value.

`pending_id_must_be_different`

The transfer was not created. Transfer.pending_id is set to the same id as
Transfer.id. Instead it should refer to a different (existing) transfer.

`timeout_reserved_for_pending_transfer`

The transfer was not created. Transfer.timeout is nonzero, but only pending
transfers have nonzero timeouts.

`closing_transfer_must_be_pending`

The transfer was not created. Transfer.flags.pending is not set, but closing
transfers must be two-phase pending transfers.

If either Transfer.flags.closing_debit or Transfer.flags.closing_credit is set,
Transfer.flags.pending must also be set.

This ensures that closing transfers are reversible by voiding the pending
transfer, and requires that the reversal operation references the corresponding
closing transfer, guarding against unexpected interleaving of close/unclose
operations.

`amount_must_not_be_zero`

Deprecated: This error code is only returned to clients prior to release 0.16.0.
Since 0.16.0, zero-amount transfers are permitted.

The transfer was not created. Transfer.amount is zero, but must be nonzero.Every
transfer must move value. Only posting and voiding transfer amounts may be zero
— when zero, they will move the full pending amount. (Client release < 0.16.0)

`ledger_must_not_be_zero`

The transfer was not created. Transfer.ledger is zero, but must be nonzero.

`code_must_not_be_zero`

The transfer was not created. Transfer.code is zero, but must be nonzero.

`debit_account_not_found`

The transfer was not created. Transfer.debit_account_id must refer to an
existing Account.

This is a transient error. The Transfer.id associated with this particular
attempt will always fail upon retry, even if the underlying issue is resolved.
To succeed, a new idempotency id must be submitted.

`credit_account_not_found`

The transfer was not created. Transfer.credit_account_id must refer to an
existing Account.

This is a transient error. The Transfer.id associated with this particular
attempt will always fail upon retry, even if the underlying issue is resolved.
To succeed, a new idempotency id must be submitted.

`accounts_must_have_the_same_ledger`

The transfer was not created. The accounts referred to by
Transfer.debit_account_id and Transfer.credit_account_id must have an identical
ledger.

Currency exchange is implemented with multiple transfers.

`transfer_must_have_the_same_ledger_as_accounts`

The transfer was not created. The accounts referred to by
Transfer.debit_account_id and Transfer.credit_account_id are equivalent, but
differ from the Transfer.ledger.

`pending_transfer_not_found`

The transfer was not created. The transfer referenced by Transfer.pending_id
does not exist.

This is a transient error. The Transfer.id associated with this particular
attempt will always fail upon retry, even if the underlying issue is resolved.
To succeed, a new idempotency id must be submitted.

`pending_transfer_not_pending`

The transfer was not created. The transfer referenced by Transfer.pending_id
exists, but does not have flags.pending set.

`pending_transfer_has_different_debit_account_id`

The transfer was not created. The transfer referenced by Transfer.pending_id
exists, but with a different debit_account_id.

The post/void transfer's debit_account_id must either be 0 or identical to the
pending transfer's debit_account_id.

`pending_transfer_has_different_credit_account_id`

The transfer was not created. The transfer referenced by Transfer.pending_id
exists, but with a different credit_account_id.

The post/void transfer's credit_account_id must either be 0 or identical to the
pending transfer's credit_account_id.

`pending_transfer_has_different_ledger`

The transfer was not created. The transfer referenced by Transfer.pending_id
exists, but with a different ledger.

The post/void transfer's ledger must either be 0 or identical to the pending
transfer's ledger.

`pending_transfer_has_different_code`

The transfer was not created. The transfer referenced by Transfer.pending_id
exists, but with a different code.

The post/void transfer's code must either be 0 or identical to the pending
transfer's code.

`exceeds_pending_transfer_amount`

The transfer was not created. The transfer's amount exceeds the amount of its
pending transfer.

`pending_transfer_has_different_amount`

The transfer was not created. The transfer is attempting to void a pending
transfer. The voiding transfer's amount must be either 0 or exactly the amount
of the pending transfer.

To partially void a transfer, create a posting transfer with an amount less than
the pending transfer's amount.

To partially void a transfer, create a posting transfer with an amount between 0
and the pending transfer's amount. (Client release < 0.16.0)

`pending_transfer_already_posted`

The transfer was not created. The referenced pending transfer was already posted
by a post_pending_transfer.

`pending_transfer_already_voided`

The transfer was not created. The referenced pending transfer was already voided
by a void_pending_transfer.

`pending_transfer_expired`

The transfer was not created. The referenced pending transfer was already voided
because its timeout has passed.

`imported_event_timestamp_must_not_regress`

This result only applies when Transfer.flags.imported is set.

The transfer was not created. The user-defined Transfer.timestamp regressed, but
it must be greater than the last timestamp assigned to any Transfer in the
cluster and cannot be equal to the timestamp of any existing Account.

`imported_event_timestamp_must_postdate_debit_account`

This result only applies when Transfer.flags.imported is set.

The transfer was not created. Transfer.debit_account_id must refer to an Account
whose timestamp is less than the Transfer.timestamp.

`imported_event_timestamp_must_postdate_credit_account`

This result only applies when Transfer.flags.imported is set.

The transfer was not created. Transfer.credit_account_id must refer to an
Account whose timestamp is less than the Transfer.timestamp.

`imported_event_timeout_must_be_zero`

This result only applies when Transfer.flags.imported is set.

The transfer was not created. The Transfer.timeout is nonzero, but must be zero.

It's possible to import pending transfers with a user-defined timestamp, but
since it's not driven by the cluster clock, it cannot define a timeout for
automatic expiration. In those cases, the two-phase post or rollback must be
done manually.

`debit_account_already_closed`

The transfer was not created. Transfer.debit_account_id must refer to an Account
whose Account.flags.closed is not already set.

This is a transient error. The Transfer.id associated with this particular
attempt will always fail upon retry, even if the underlying issue is resolved.
To succeed, a new idempotency id must be submitted.

`credit_account_already_closed`

The transfer was not created. Transfer.credit_account_id must refer to an Account
whose Account.flags.closed is not already set.

This is a transient error. The Transfer.id associated with this particular
attempt will always fail upon retry, even if the underlying issue is resolved.
To succeed, a new idempotency id must be submitted.

`overflows_debits_pending`

The transfer was not created. debit_account.debits_pending + transfer.amount
would overflow a 128-bit unsigned integer.

`overflows_credits_pending`

The transfer was not created. credit_account.credits_pending + transfer.amount
would overflow a 128-bit unsigned integer.

`overflows_debits_posted`

The transfer was not created. debit_account.debits_posted + transfer.amount
would overflow a 128-bit unsigned integer.

`overflows_credits_posted`

The transfer was not created. debit_account.credits_posted + transfer.amount
would overflow a 128-bit unsigned integer.

`overflows_debits`

The transfer was not created. debit_account.debits_pending +
debit_account.debits_posted + transfer.amount would overflow a 128-bit unsigned
integer.

`overflows_credits`

The transfer was not created. credit_account.credits_pending +
credit_account.credits_posted + transfer.amount would overflow a 128-bit
unsigned integer.

`overflows_timeout`

The transfer was not created. transfer.timestamp + (transfer.timeout *
1_000_000_000) would exceed 2^63.

Transfer.timeout is converted to nanoseconds.

This computation uses the Transfer.timestamp value assigned by the replica, not
the 0 value sent by the client.

`exceeds_credits`

The transfer was not created.

The debit account has flags.debits_must_not_exceed_credits set, but
debit_account.debits_pending + debit_account.debits_posted + transfer.amount
would exceed debit_account.credits_posted.

This is a transient error. The Transfer.id associated with this particular
attempt will always fail upon retry, even if the underlying issue is resolved.
To succeed, a new idempotency id must be submitted.

If flags.balancing_debit is set, then debit_account.debits_pending +
debit_account.debits_posted + 1 would exceed debit_account.credits_posted.
(Client release < 0.16.0)

`exceeds_debits`

The transfer was not created.

The credit account has flags.credits_must_not_exceed_debits set, but
credit_account.credits_pending + credit_account.credits_posted + transfer.amount
would exceed credit_account.debits_posted.

This is a transient error. The Transfer.id associated with this particular
attempt will always fail upon retry, even if the underlying issue is resolved.
To succeed, a new idempotency id must be submitted.

If flags.balancing_credit is set, then credit_account.credits_pending +
credit_account.credits_posted + 1 would exceed credit_account.debits_posted.
(Client release < 0.16.0)
-}
createTransfers :: C.Client -> [I.Transfer] -> IO [I.CreateTransfersResult]
createTransfers client = sendRequest client I.OPERATION_CREATE_TRANSFERS

echoCreateTransfers :: C.Client -> [I.Transfer] -> IO [I.Transfer]
echoCreateTransfers client = echoRequest client I.OPERATION_CREATE_TRANSFERS

{- | Fetch one or more accounts by their ids.

Note that you should not check an account's balance using this request before
creating a transfer. That would not be atomic and the balance could change in
between the check and the transfer. Instead, set the
debits_must_not_exceed_credits or credits_must_not_exceed_debits flag on the
accounts to limit their account balances. More complex conditional transfers can
be expressed using balance-conditional transfers.

It is not possible currently to look up more than a full batch (8190) of
accounts atomically. When issuing multiple lookup_accounts calls, it can happen
that other operations will interleave between the calls leading to read skew.
Consider using the history flag to enable atomic lookups.

Result

  * If the account exists, return the Account.
  * If the account does not exist, return nothing.
-}
lookupAccounts :: C.Client -> [AccountId] -> IO [I.Account]
lookupAccounts client = sendRequest client I.OPERATION_LOOKUP_ACCOUNTS

echoLookupAccounts :: C.Client -> [AccountId] -> IO [AccountId]
echoLookupAccounts client = echoRequest client I.OPERATION_LOOKUP_ACCOUNTS

{- | Fetch one or more transfers by their ids.

Result

  * If the transfer exists, return the Transfer.
  * If the transfer does not exist, return nothing.
-}
lookupTransfers :: C.Client -> [TransferId] -> IO [I.Transfer]
lookupTransfers client = sendRequest client I.OPERATION_LOOKUP_TRANSFERS

echoLookupTransfers :: C.Client -> [TransferId] -> IO [TransferId]
echoLookupTransfers client = echoRequest client I.OPERATION_LOOKUP_TRANSFERS

{- | Fetch Transfers involving a given Account.

Result

  * Return a (possibly empty) array of Transfers that match the filter.
  * If any constraint is violated, return nothing.
  * By default, Transfers are sorted chronologically by timestamp. You can use
    the reversed to change this.
  * The result is always limited in size. If there are more results, you need to
    page through them using the AccountFilter's timestamp_min and/or
    timestamp_max.
-}
getAccountTransfers :: C.Client -> I.AccountFilter -> IO [I.Transfer]
getAccountTransfers client = sendRequest client I.OPERATION_GET_ACCOUNT_TRANSFERS

echoGetAccountTransfers :: C.Client -> I.AccountFilter -> IO I.AccountFilter
echoGetAccountTransfers client = echoRequest client I.OPERATION_GET_ACCOUNT_TRANSFERS

{- | Fetch the historical AccountBalances of a given Account.

Only accounts created with the history flag set retain historical balances. This
is off by default.

  * Each balance returned has a corresponding transfer with the same timestamp.
    See the get_account_transfers operation for more details.
  * The amounts refer to the account balance recorded after the transfer
    execution.
  * Pending balances automatically removed due to timeout expiration don't
    change historical balances.

Result

  * If the account has the flag history set and any matching balances exist,
    return an array of AccountBalances.
  * If the account does not have the flag history set, return nothing.
  * If no matching balances exist, return nothing.
  * If any constraint is violated, return nothing.
-}
getAccountBalances :: C.Client -> I.AccountFilter -> IO [I.AccountBalance]
getAccountBalances client = sendRequest client I.OPERATION_GET_ACCOUNT_BALANCES

echoGetAccountBalances :: C.Client -> I.AccountFilter -> IO I.AccountFilter
echoGetAccountBalances client = echoRequest client I.OPERATION_GET_ACCOUNT_BALANCES

{- | Query Accounts by the intersection of some fields and by timestamp range.

It is not possible currently to query more than a full batch (8190) of accounts
atomically. When issuing multiple query_accounts calls, it can happen that other
operations will interleave between the calls leading to read skew. Consider
using the history flag to enable atomic lookups.

Result

 * Return a (possibly empty) array of Accounts that match the filter.
 * If any constraint is violated, return nothing.
 * By default, Accounts are sorted chronologically by timestamp. You can use the
   reversed to change this.
 * The result is always limited in size. If there are more results, you need to
   page through them using the QueryFilter's timestamp_min and/or timestamp_max.
-}
queryAccounts :: C.Client -> I.AccountFilter -> IO [I.Account]
queryAccounts client = sendRequest client I.OPERATION_QUERY_ACCOUNTS

echoQueryAccounts :: C.Client -> I.AccountFilter -> IO I.AccountFilter
echoQueryAccounts client = echoRequest client I.OPERATION_QUERY_ACCOUNTS

{- | Query Transfers by the intersection of some fields and by timestamp range.

Result

  * Return a (possibly empty) array of Transfers that match the filter.
  * If any constraint is violated, return nothing.
  * By default, Transfers are sorted chronologically by timestamp. You can use
    the reversed to change this.
  * The result is always limited in size. If there are more results, you need to
    page through them using the QueryFilter's timestamp_min and/or timestamp_max.
-}
queryTransfers :: C.Client -> I.QueryFilter -> IO [I.Transfer]
queryTransfers client = sendRequest client I.OPERATION_QUERY_TRANSFERS

echoQueryTransfers :: C.Client -> I.QueryFilter -> IO I.QueryFilter
echoQueryTransfers client = echoRequest client I.OPERATION_QUERY_TRANSFERS
