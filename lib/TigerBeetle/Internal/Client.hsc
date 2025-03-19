{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

module TigerBeetle.Internal.Client where

import Data.Bits
import Data.Primitive.Types
import Data.WideWord
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (id)

#include <tb_client.h>

newtype AccountFlags = AccountFlags Word16 deriving (Eq, Ord, Show, Read, Num, Enum, Real, Integral, Storable, Prim, Bits)

pattern ACCOUNT_LINKED = (#const TB_ACCOUNT_LINKED) :: AccountFlags
pattern ACCOUNT_DEBITS_MUST_NOT_EXCEED_CREDITS = (#const TB_ACCOUNT_DEBITS_MUST_NOT_EXCEED_CREDITS) :: AccountFlags
pattern ACCOUNT_CREDITS_MUST_NOT_EXCEED_DEBITS = (#const TB_ACCOUNT_CREDITS_MUST_NOT_EXCEED_DEBITS) :: AccountFlags
pattern ACCOUNT_HISTORY = (#const TB_ACCOUNT_HISTORY) :: AccountFlags
pattern ACCOUNT_IMPORTED = (#const TB_ACCOUNT_IMPORTED) :: AccountFlags
pattern ACCOUNT_CLOSED = (#const TB_ACCOUNT_CLOSED) :: AccountFlags

{-# complete ACCOUNT_LINKED, ACCOUNT_DEBITS_MUST_NOT_EXCEED_CREDITS, ACCOUNT_CREDITS_MUST_NOT_EXCEED_DEBITS, ACCOUNT_HISTORY, ACCOUNT_IMPORTED, ACCOUNT_CLOSED #-}

data Account = Account
  { id :: {-# UNPACK #-} !Word128,
    debits_pending :: {-# UNPACK #-} !Word128,
    debits_posted :: {-# UNPACK #-} !Word128,
    credits_pending :: {-# UNPACK #-} !Word128,
    credits_posted :: {-# UNPACK #-} !Word128,
    user_data_128 :: {-# UNPACK #-} !Word128,
    user_data_64 :: {-# UNPACK #-} !Word64,
    user_data_32 :: {-# UNPACK #-} !Word32,
    ledger :: {-# UNPACK #-} !Word32,
    code :: {-# UNPACK #-} !Word16,
    flags :: {-# UNPACK #-} !AccountFlags,
    timestamp :: {-# UNPACK #-} !Word64
  }
  deriving (Show, Read, Eq, Ord)

instance Storable Account where
  sizeOf _ = (#size tb_account_t)
  alignment _ = (#alignment tb_account_t)
  peek ptr = do
    id <- (#peek tb_account_t, id) ptr
    debits_pending <- (#peek tb_account_t, debits_pending) ptr
    debits_posted <- (#peek tb_account_t, debits_posted) ptr
    credits_pending <- (#peek tb_account_t, credits_pending) ptr
    credits_posted <- (#peek tb_account_t, credits_posted) ptr
    user_data_128 <- (#peek tb_account_t, user_data_128) ptr
    user_data_64 <- (#peek tb_account_t, user_data_64) ptr
    user_data_32 <- (#peek tb_account_t, user_data_32) ptr
    ledger <- (#peek tb_account_t, ledger) ptr
    code <- (#peek tb_account_t, code) ptr
    flags <- (#peek tb_account_t, flags) ptr
    timestamp <- (#peek tb_account_t, timestamp) ptr
    pure Account {..}
  poke ptr Account {..} = do
    (#poke tb_account_t, id) ptr id
    (#poke tb_account_t, debits_pending) ptr debits_pending
    (#poke tb_account_t, debits_posted) ptr debits_posted
    (#poke tb_account_t, credits_pending) ptr credits_pending
    (#poke tb_account_t, credits_posted) ptr credits_posted
    (#poke tb_account_t, user_data_128) ptr user_data_128
    (#poke tb_account_t, user_data_64) ptr user_data_64
    (#poke tb_account_t, user_data_32) ptr user_data_32
    pokeByteOff ptr (#offset tb_account_t, reserved) (0 :: Word32) 
    (#poke tb_account_t, ledger) ptr ledger
    (#poke tb_account_t, code) ptr code
    (#poke tb_account_t, flags) ptr flags
    (#poke tb_account_t, timestamp) ptr timestamp

newtype TransferFlags = TransferFlags Word16 deriving (Eq, Ord, Show, Read, Num, Enum, Real, Integral, Storable, Prim, Bits)

pattern TRANSFER_LINKED = (#const TB_TRANSFER_LINKED) :: TransferFlags
pattern TRANSFER_PENDING = (#const TB_TRANSFER_PENDING) :: TransferFlags
pattern TRANSFER_POST_PENDING_TRANSFER = (#const TB_TRANSFER_POST_PENDING_TRANSFER) :: TransferFlags
pattern TRANSFER_VOID_PENDING_TRANSFER = (#const TB_TRANSFER_VOID_PENDING_TRANSFER) :: TransferFlags
pattern TRANSFER_BALANCING_DEBIT = (#const TB_TRANSFER_BALANCING_DEBIT) :: TransferFlags
pattern TRANSFER_BALANCING_CREDIT = (#const TB_TRANSFER_BALANCING_CREDIT) :: TransferFlags
pattern TRANSFER_CLOSING_DEBIT = (#const TB_TRANSFER_CLOSING_DEBIT) :: TransferFlags
pattern TRANSFER_CLOSING_CREDIT = (#const TB_TRANSFER_CLOSING_CREDIT) :: TransferFlags
pattern TRANSFER_IMPORTED = (#const TB_TRANSFER_IMPORTED) :: TransferFlags

{-# complete TRANSFER_LINKED, TRANSFER_PENDING, TRANSFER_POST_PENDING_TRANSFER, TRANSFER_VOID_PENDING_TRANSFER, TRANSFER_BALANCING_DEBIT, TRANSFER_BALANCING_CREDIT, TRANSFER_CLOSING_DEBIT, TRANSFER_CLOSING_CREDIT, TRANSFER_IMPORTED #-}

data Transfer = Transfer
  { id :: {-# UNPACK #-} !Word128,
    debit_account_id :: {-# UNPACK #-} !Word128,
    credit_account_id :: {-# UNPACK #-} !Word128,
    amount :: {-# UNPACK #-} !Word128,
    pending_id :: {-# UNPACK #-} !Word128,
    user_data_128 :: {-# UNPACK #-} !Word128,
    user_data_64 :: {-# UNPACK #-} !Word64,
    user_data_32 :: {-# UNPACK #-} !Word32,
    timeout :: {-# UNPACK #-} !Word32,
    ledger :: {-# UNPACK #-} !Word32,
    code :: {-# UNPACK #-} !Word16,
    flags :: {-# UNPACK #-} !TransferFlags,
    timestamp :: {-# UNPACK #-} !Word64
  }
  deriving (Show, Read, Eq, Ord)

instance Storable Transfer where
  sizeOf _ = (#size tb_transfer_t)
  alignment _ = (#alignment tb_transfer_t)
  peek ptr = do
    id <- (#peek tb_transfer_t, id) ptr
    debit_account_id <- (#peek tb_transfer_t, debit_account_id) ptr
    credit_account_id <- (#peek tb_transfer_t, credit_account_id) ptr
    amount <- (#peek tb_transfer_t, amount) ptr
    pending_id <- (#peek tb_transfer_t, pending_id) ptr
    user_data_128 <- (#peek tb_transfer_t, user_data_128) ptr
    user_data_64 <- (#peek tb_transfer_t, user_data_64) ptr
    user_data_32 <- (#peek tb_transfer_t, user_data_32) ptr
    timeout <- (#peek tb_transfer_t, timeout) ptr
    ledger <- (#peek tb_transfer_t, ledger) ptr
    code <- (#peek tb_transfer_t, code) ptr
    flags <- (#peek tb_transfer_t, flags) ptr
    timestamp <- (#peek tb_transfer_t, timestamp) ptr
    return Transfer {..}
  poke ptr Transfer {..} = do
    (#poke tb_transfer_t, id) ptr id
    (#poke tb_transfer_t, debit_account_id) ptr debit_account_id
    (#poke tb_transfer_t, credit_account_id) ptr credit_account_id
    (#poke tb_transfer_t, amount) ptr amount
    (#poke tb_transfer_t, pending_id) ptr pending_id
    (#poke tb_transfer_t, user_data_128) ptr user_data_128
    (#poke tb_transfer_t, user_data_64) ptr user_data_64
    (#poke tb_transfer_t, user_data_32) ptr user_data_32
    (#poke tb_transfer_t, timeout) ptr timeout
    (#poke tb_transfer_t, ledger) ptr ledger
    (#poke tb_transfer_t, code) ptr code
    (#poke tb_transfer_t, flags) ptr flags
    (#poke tb_transfer_t, timestamp) ptr timestamp

newtype CreateAccountResult = CreateAccountResult CInt deriving (Eq, Ord, Show, Read, Num, Enum, Real, Integral, Storable, Prim, Bits)

pattern CREATE_ACCOUNT_OK = (#const TB_CREATE_ACCOUNT_OK) :: CreateAccountResult
pattern CREATE_ACCOUNT_LINKED_EVENT_FAILED = (#const TB_CREATE_ACCOUNT_LINKED_EVENT_FAILED) :: CreateAccountResult
pattern CREATE_ACCOUNT_LINKED_EVENT_CHAIN_OPEN = (#const TB_CREATE_ACCOUNT_LINKED_EVENT_CHAIN_OPEN) :: CreateAccountResult
pattern CREATE_ACCOUNT_IMPORTED_EVENT_EXPECTED = (#const TB_CREATE_ACCOUNT_IMPORTED_EVENT_EXPECTED) :: CreateAccountResult
pattern CREATE_ACCOUNT_IMPORTED_EVENT_NOT_EXPECTED = (#const TB_CREATE_ACCOUNT_IMPORTED_EVENT_NOT_EXPECTED) :: CreateAccountResult
pattern CREATE_ACCOUNT_TIMESTAMP_MUST_BE_ZERO = (#const TB_CREATE_ACCOUNT_TIMESTAMP_MUST_BE_ZERO) :: CreateAccountResult
pattern CREATE_ACCOUNT_IMPORTED_EVENT_TIMESTAMP_OUT_OF_RANGE = (#const TB_CREATE_ACCOUNT_IMPORTED_EVENT_TIMESTAMP_OUT_OF_RANGE) :: CreateAccountResult
pattern CREATE_ACCOUNT_IMPORTED_EVENT_TIMESTAMP_MUST_NOT_ADVANCE = (#const TB_CREATE_ACCOUNT_IMPORTED_EVENT_TIMESTAMP_MUST_NOT_ADVANCE) :: CreateAccountResult
pattern CREATE_ACCOUNT_RESERVED_FIELD = (#const TB_CREATE_ACCOUNT_RESERVED_FIELD) :: CreateAccountResult
pattern CREATE_ACCOUNT_RESERVED_FLAG = (#const TB_CREATE_ACCOUNT_RESERVED_FLAG) :: CreateAccountResult
pattern CREATE_ACCOUNT_ID_MUST_NOT_BE_ZERO = (#const TB_CREATE_ACCOUNT_ID_MUST_NOT_BE_ZERO) :: CreateAccountResult
pattern CREATE_ACCOUNT_ID_MUST_NOT_BE_INT_MAX = (#const TB_CREATE_ACCOUNT_ID_MUST_NOT_BE_INT_MAX) :: CreateAccountResult
pattern CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_FLAGS = (#const TB_CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_FLAGS) :: CreateAccountResult
pattern CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_USER_DATA_128 = (#const TB_CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_USER_DATA_128) :: CreateAccountResult
pattern CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_USER_DATA_64 = (#const TB_CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_USER_DATA_64) :: CreateAccountResult
pattern CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_USER_DATA_32 = (#const TB_CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_USER_DATA_32) :: CreateAccountResult
pattern CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_LEDGER = (#const TB_CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_LEDGER) :: CreateAccountResult
pattern CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_CODE = (#const TB_CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_CODE) :: CreateAccountResult
pattern CREATE_ACCOUNT_EXISTS = (#const TB_CREATE_ACCOUNT_EXISTS) :: CreateAccountResult
pattern CREATE_ACCOUNT_FLAGS_ARE_MUTUALLY_EXCLUSIVE = (#const TB_CREATE_ACCOUNT_FLAGS_ARE_MUTUALLY_EXCLUSIVE) :: CreateAccountResult
pattern CREATE_ACCOUNT_DEBITS_PENDING_MUST_BE_ZERO = (#const TB_CREATE_ACCOUNT_DEBITS_PENDING_MUST_BE_ZERO) :: CreateAccountResult
pattern CREATE_ACCOUNT_DEBITS_POSTED_MUST_BE_ZERO = (#const TB_CREATE_ACCOUNT_DEBITS_POSTED_MUST_BE_ZERO) :: CreateAccountResult
pattern CREATE_ACCOUNT_CREDITS_PENDING_MUST_BE_ZERO = (#const TB_CREATE_ACCOUNT_CREDITS_PENDING_MUST_BE_ZERO) :: CreateAccountResult
pattern CREATE_ACCOUNT_CREDITS_POSTED_MUST_BE_ZERO = (#const TB_CREATE_ACCOUNT_CREDITS_POSTED_MUST_BE_ZERO) :: CreateAccountResult
pattern CREATE_ACCOUNT_LEDGER_MUST_NOT_BE_ZERO = (#const TB_CREATE_ACCOUNT_LEDGER_MUST_NOT_BE_ZERO) :: CreateAccountResult
pattern CREATE_ACCOUNT_CODE_MUST_NOT_BE_ZERO = (#const TB_CREATE_ACCOUNT_CODE_MUST_NOT_BE_ZERO) :: CreateAccountResult
pattern CREATE_ACCOUNT_IMPORTED_EVENT_TIMESTAMP_MUST_NOT_REGRESS = (#const TB_CREATE_ACCOUNT_IMPORTED_EVENT_TIMESTAMP_MUST_NOT_REGRESS) :: CreateAccountResult

{-# complete CREATE_ACCOUNT_OK, CREATE_ACCOUNT_LINKED_EVENT_FAILED, CREATE_ACCOUNT_LINKED_EVENT_CHAIN_OPEN, CREATE_ACCOUNT_IMPORTED_EVENT_EXPECTED, CREATE_ACCOUNT_IMPORTED_EVENT_NOT_EXPECTED, CREATE_ACCOUNT_TIMESTAMP_MUST_BE_ZERO, CREATE_ACCOUNT_IMPORTED_EVENT_TIMESTAMP_OUT_OF_RANGE, CREATE_ACCOUNT_IMPORTED_EVENT_TIMESTAMP_MUST_NOT_ADVANCE, CREATE_ACCOUNT_RESERVED_FIELD, CREATE_ACCOUNT_RESERVED_FLAG, CREATE_ACCOUNT_ID_MUST_NOT_BE_ZERO, CREATE_ACCOUNT_ID_MUST_NOT_BE_INT_MAX, CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_FLAGS, CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_USER_DATA_128, CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_USER_DATA_64, CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_USER_DATA_32, CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_LEDGER, CREATE_ACCOUNT_EXISTS_WITH_DIFFERENT_CODE, CREATE_ACCOUNT_EXISTS, CREATE_ACCOUNT_FLAGS_ARE_MUTUALLY_EXCLUSIVE, CREATE_ACCOUNT_DEBITS_PENDING_MUST_BE_ZERO, CREATE_ACCOUNT_DEBITS_POSTED_MUST_BE_ZERO, CREATE_ACCOUNT_CREDITS_PENDING_MUST_BE_ZERO, CREATE_ACCOUNT_CREDITS_POSTED_MUST_BE_ZERO, CREATE_ACCOUNT_LEDGER_MUST_NOT_BE_ZERO, CREATE_ACCOUNT_CODE_MUST_NOT_BE_ZERO, CREATE_ACCOUNT_IMPORTED_EVENT_TIMESTAMP_MUST_NOT_REGRESS #-}

newtype CreateTransferResult = CreateTransferResult CInt deriving (Eq, Ord, Show, Read, Num, Enum, Real, Integral, Storable, Prim, Bits)

pattern CREATE_TRANSFER_OK = (#const TB_CREATE_TRANSFER_OK) :: CreateTransferResult
pattern CREATE_TRANSFER_LINKED_EVENT_FAILED = (#const TB_CREATE_TRANSFER_LINKED_EVENT_FAILED) :: CreateTransferResult
pattern CREATE_TRANSFER_LINKED_EVENT_CHAIN_OPEN = (#const TB_CREATE_TRANSFER_LINKED_EVENT_CHAIN_OPEN) :: CreateTransferResult
pattern CREATE_TRANSFER_IMPORTED_EVENT_EXPECTED = (#const TB_CREATE_TRANSFER_IMPORTED_EVENT_EXPECTED) :: CreateTransferResult
pattern CREATE_TRANSFER_IMPORTED_EVENT_NOT_EXPECTED = (#const TB_CREATE_TRANSFER_IMPORTED_EVENT_NOT_EXPECTED) :: CreateTransferResult
pattern CREATE_TRANSFER_TIMESTAMP_MUST_BE_ZERO = (#const TB_CREATE_TRANSFER_TIMESTAMP_MUST_BE_ZERO) :: CreateTransferResult
pattern CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_OUT_OF_RANGE = (#const TB_CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_OUT_OF_RANGE) :: CreateTransferResult
pattern CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_MUST_NOT_ADVANCE = (#const TB_CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_MUST_NOT_ADVANCE) :: CreateTransferResult
pattern CREATE_TRANSFER_RESERVED_FLAG = (#const TB_CREATE_TRANSFER_RESERVED_FLAG) :: CreateTransferResult
pattern CREATE_TRANSFER_ID_MUST_NOT_BE_ZERO = (#const TB_CREATE_TRANSFER_ID_MUST_NOT_BE_ZERO) :: CreateTransferResult
pattern CREATE_TRANSFER_ID_MUST_NOT_BE_INT_MAX = (#const TB_CREATE_TRANSFER_ID_MUST_NOT_BE_INT_MAX) :: CreateTransferResult
pattern CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_FLAGS = (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_FLAGS) :: CreateTransferResult
pattern CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_PENDING_ID = (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_PENDING_ID) :: CreateTransferResult
pattern CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_TIMEOUT = (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_TIMEOUT) :: CreateTransferResult
pattern CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_DEBIT_ACCOUNT_ID = (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_DEBIT_ACCOUNT_ID) :: CreateTransferResult
pattern CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_CREDIT_ACCOUNT_ID = (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_CREDIT_ACCOUNT_ID) :: CreateTransferResult
pattern CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_AMOUNT = (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_AMOUNT) :: CreateTransferResult
pattern CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_USER_DATA_128 = (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_USER_DATA_128) :: CreateTransferResult
pattern CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_USER_DATA_64 = (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_USER_DATA_64) :: CreateTransferResult
pattern CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_USER_DATA_32 = (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_USER_DATA_32) :: CreateTransferResult
pattern CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_LEDGER = (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_LEDGER) :: CreateTransferResult
pattern CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_CODE = (#const TB_CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_CODE) :: CreateTransferResult
pattern CREATE_TRANSFER_EXISTS = (#const TB_CREATE_TRANSFER_EXISTS) :: CreateTransferResult
pattern CREATE_TRANSFER_ID_ALREADY_FAILED = (#const TB_CREATE_TRANSFER_ID_ALREADY_FAILED) :: CreateTransferResult
pattern CREATE_TRANSFER_FLAGS_ARE_MUTUALLY_EXCLUSIVE = (#const TB_CREATE_TRANSFER_FLAGS_ARE_MUTUALLY_EXCLUSIVE) :: CreateTransferResult
pattern CREATE_TRANSFER_DEBIT_ACCOUNT_ID_MUST_NOT_BE_ZERO = (#const TB_CREATE_TRANSFER_DEBIT_ACCOUNT_ID_MUST_NOT_BE_ZERO) :: CreateTransferResult
pattern CREATE_TRANSFER_DEBIT_ACCOUNT_ID_MUST_NOT_BE_INT_MAX = (#const TB_CREATE_TRANSFER_DEBIT_ACCOUNT_ID_MUST_NOT_BE_INT_MAX) :: CreateTransferResult
pattern CREATE_TRANSFER_CREDIT_ACCOUNT_ID_MUST_NOT_BE_ZERO = (#const TB_CREATE_TRANSFER_CREDIT_ACCOUNT_ID_MUST_NOT_BE_ZERO) :: CreateTransferResult
pattern CREATE_TRANSFER_CREDIT_ACCOUNT_ID_MUST_NOT_BE_INT_MAX = (#const TB_CREATE_TRANSFER_CREDIT_ACCOUNT_ID_MUST_NOT_BE_INT_MAX) :: CreateTransferResult
pattern CREATE_TRANSFER_ACCOUNTS_MUST_BE_DIFFERENT = (#const TB_CREATE_TRANSFER_ACCOUNTS_MUST_BE_DIFFERENT) :: CreateTransferResult
pattern CREATE_TRANSFER_PENDING_ID_MUST_BE_ZERO = (#const TB_CREATE_TRANSFER_PENDING_ID_MUST_BE_ZERO) :: CreateTransferResult
pattern CREATE_TRANSFER_PENDING_ID_MUST_NOT_BE_ZERO = (#const TB_CREATE_TRANSFER_PENDING_ID_MUST_NOT_BE_ZERO) :: CreateTransferResult
pattern CREATE_TRANSFER_PENDING_ID_MUST_NOT_BE_INT_MAX = (#const TB_CREATE_TRANSFER_PENDING_ID_MUST_NOT_BE_INT_MAX) :: CreateTransferResult
pattern CREATE_TRANSFER_PENDING_ID_MUST_BE_DIFFERENT = (#const TB_CREATE_TRANSFER_PENDING_ID_MUST_BE_DIFFERENT) :: CreateTransferResult
pattern CREATE_TRANSFER_TIMEOUT_RESERVED_FOR_PENDING_TRANSFER = (#const TB_CREATE_TRANSFER_TIMEOUT_RESERVED_FOR_PENDING_TRANSFER) :: CreateTransferResult
pattern CREATE_TRANSFER_CLOSING_TRANSFER_MUST_BE_PENDING = (#const TB_CREATE_TRANSFER_CLOSING_TRANSFER_MUST_BE_PENDING) :: CreateTransferResult
pattern CREATE_TRANSFER_AMOUNT_MUST_NOT_BE_ZERO = (#const TB_CREATE_TRANSFER_AMOUNT_MUST_NOT_BE_ZERO) :: CreateTransferResult
pattern CREATE_TRANSFER_LEDGER_MUST_NOT_BE_ZERO = (#const TB_CREATE_TRANSFER_LEDGER_MUST_NOT_BE_ZERO) :: CreateTransferResult
pattern CREATE_TRANSFER_CODE_MUST_NOT_BE_ZERO = (#const TB_CREATE_TRANSFER_CODE_MUST_NOT_BE_ZERO) :: CreateTransferResult
pattern CREATE_TRANSFER_DEBIT_ACCOUNT_NOT_FOUND = (#const TB_CREATE_TRANSFER_DEBIT_ACCOUNT_NOT_FOUND) :: CreateTransferResult
pattern CREATE_TRANSFER_CREDIT_ACCOUNT_NOT_FOUND = (#const TB_CREATE_TRANSFER_CREDIT_ACCOUNT_NOT_FOUND) :: CreateTransferResult
pattern CREATE_TRANSFER_ACCOUNTS_MUST_HAVE_THE_SAME_LEDGER = (#const TB_CREATE_TRANSFER_ACCOUNTS_MUST_HAVE_THE_SAME_LEDGER) :: CreateTransferResult
pattern CREATE_TRANSFER_TRANSFER_MUST_HAVE_THE_SAME_LEDGER_AS_ACCOUNTS = (#const TB_CREATE_TRANSFER_TRANSFER_MUST_HAVE_THE_SAME_LEDGER_AS_ACCOUNTS) :: CreateTransferResult
pattern CREATE_TRANSFER_PENDING_TRANSFER_NOT_FOUND = (#const TB_CREATE_TRANSFER_PENDING_TRANSFER_NOT_FOUND) :: CreateTransferResult
pattern CREATE_TRANSFER_PENDING_TRANSFER_NOT_PENDING = (#const TB_CREATE_TRANSFER_PENDING_TRANSFER_NOT_PENDING) :: CreateTransferResult
pattern CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_DEBIT_ACCOUNT_ID = (#const TB_CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_DEBIT_ACCOUNT_ID) :: CreateTransferResult
pattern CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_CREDIT_ACCOUNT_ID = (#const TB_CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_CREDIT_ACCOUNT_ID) :: CreateTransferResult
pattern CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_LEDGER = (#const TB_CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_LEDGER) :: CreateTransferResult
pattern CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_CODE = (#const TB_CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_CODE) :: CreateTransferResult
pattern CREATE_TRANSFER_EXCEEDS_PENDING_TRANSFER_AMOUNT = (#const TB_CREATE_TRANSFER_EXCEEDS_PENDING_TRANSFER_AMOUNT) :: CreateTransferResult
pattern CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_AMOUNT = (#const TB_CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_AMOUNT) :: CreateTransferResult
pattern CREATE_TRANSFER_PENDING_TRANSFER_ALREADY_POSTED = (#const TB_CREATE_TRANSFER_PENDING_TRANSFER_ALREADY_POSTED) :: CreateTransferResult
pattern CREATE_TRANSFER_PENDING_TRANSFER_ALREADY_VOIDED = (#const TB_CREATE_TRANSFER_PENDING_TRANSFER_ALREADY_VOIDED) :: CreateTransferResult
pattern CREATE_TRANSFER_PENDING_TRANSFER_EXPIRED = (#const TB_CREATE_TRANSFER_PENDING_TRANSFER_EXPIRED) :: CreateTransferResult
pattern CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_MUST_NOT_REGRESS = (#const TB_CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_MUST_NOT_REGRESS) :: CreateTransferResult
pattern CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_MUST_POSTDATE_DEBIT_ACCOUNT = (#const TB_CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_MUST_POSTDATE_DEBIT_ACCOUNT) :: CreateTransferResult
pattern CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_MUST_POSTDATE_CREDIT_ACCOUNT = (#const TB_CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_MUST_POSTDATE_CREDIT_ACCOUNT) :: CreateTransferResult
pattern CREATE_TRANSFER_IMPORTED_EVENT_TIMEOUT_MUST_BE_ZERO = (#const TB_CREATE_TRANSFER_IMPORTED_EVENT_TIMEOUT_MUST_BE_ZERO) :: CreateTransferResult
pattern CREATE_TRANSFER_DEBIT_ACCOUNT_ALREADY_CLOSED = (#const TB_CREATE_TRANSFER_DEBIT_ACCOUNT_ALREADY_CLOSED) :: CreateTransferResult
pattern CREATE_TRANSFER_CREDIT_ACCOUNT_ALREADY_CLOSED = (#const TB_CREATE_TRANSFER_CREDIT_ACCOUNT_ALREADY_CLOSED) :: CreateTransferResult
pattern CREATE_TRANSFER_OVERFLOWS_DEBITS_PENDING = (#const TB_CREATE_TRANSFER_OVERFLOWS_DEBITS_PENDING) :: CreateTransferResult
pattern CREATE_TRANSFER_OVERFLOWS_CREDITS_PENDING = (#const TB_CREATE_TRANSFER_OVERFLOWS_CREDITS_PENDING) :: CreateTransferResult
pattern CREATE_TRANSFER_OVERFLOWS_DEBITS_POSTED = (#const TB_CREATE_TRANSFER_OVERFLOWS_DEBITS_POSTED) :: CreateTransferResult
pattern CREATE_TRANSFER_OVERFLOWS_CREDITS_POSTED = (#const TB_CREATE_TRANSFER_OVERFLOWS_CREDITS_POSTED) :: CreateTransferResult
pattern CREATE_TRANSFER_OVERFLOWS_DEBITS = (#const TB_CREATE_TRANSFER_OVERFLOWS_DEBITS) :: CreateTransferResult
pattern CREATE_TRANSFER_OVERFLOWS_CREDITS = (#const TB_CREATE_TRANSFER_OVERFLOWS_CREDITS) :: CreateTransferResult
pattern CREATE_TRANSFER_OVERFLOWS_TIMEOUT = (#const TB_CREATE_TRANSFER_OVERFLOWS_TIMEOUT) :: CreateTransferResult
pattern CREATE_TRANSFER_EXCEEDS_CREDITS = (#const TB_CREATE_TRANSFER_EXCEEDS_CREDITS) :: CreateTransferResult
pattern CREATE_TRANSFER_EXCEEDS_DEBITS = (#const TB_CREATE_TRANSFER_EXCEEDS_DEBITS) :: CreateTransferResult

{-# complete CREATE_TRANSFER_OK, CREATE_TRANSFER_LINKED_EVENT_FAILED, CREATE_TRANSFER_LINKED_EVENT_CHAIN_OPEN, CREATE_TRANSFER_IMPORTED_EVENT_EXPECTED, CREATE_TRANSFER_IMPORTED_EVENT_NOT_EXPECTED, CREATE_TRANSFER_TIMESTAMP_MUST_BE_ZERO, CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_OUT_OF_RANGE, CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_MUST_NOT_ADVANCE, CREATE_TRANSFER_RESERVED_FLAG, CREATE_TRANSFER_ID_MUST_NOT_BE_ZERO, CREATE_TRANSFER_ID_MUST_NOT_BE_INT_MAX, CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_FLAGS, CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_PENDING_ID, CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_TIMEOUT, CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_DEBIT_ACCOUNT_ID, CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_CREDIT_ACCOUNT_ID, CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_AMOUNT, CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_USER_DATA_128, CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_USER_DATA_64, CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_USER_DATA_32, CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_LEDGER, CREATE_TRANSFER_EXISTS_WITH_DIFFERENT_CODE, CREATE_TRANSFER_EXISTS, CREATE_TRANSFER_ID_ALREADY_FAILED, CREATE_TRANSFER_FLAGS_ARE_MUTUALLY_EXCLUSIVE, CREATE_TRANSFER_DEBIT_ACCOUNT_ID_MUST_NOT_BE_ZERO, CREATE_TRANSFER_DEBIT_ACCOUNT_ID_MUST_NOT_BE_INT_MAX, CREATE_TRANSFER_CREDIT_ACCOUNT_ID_MUST_NOT_BE_ZERO, CREATE_TRANSFER_CREDIT_ACCOUNT_ID_MUST_NOT_BE_INT_MAX, CREATE_TRANSFER_ACCOUNTS_MUST_BE_DIFFERENT, CREATE_TRANSFER_PENDING_ID_MUST_BE_ZERO, CREATE_TRANSFER_PENDING_ID_MUST_NOT_BE_ZERO, CREATE_TRANSFER_PENDING_ID_MUST_NOT_BE_INT_MAX, CREATE_TRANSFER_PENDING_ID_MUST_BE_DIFFERENT, CREATE_TRANSFER_TIMEOUT_RESERVED_FOR_PENDING_TRANSFER, CREATE_TRANSFER_CLOSING_TRANSFER_MUST_BE_PENDING, CREATE_TRANSFER_AMOUNT_MUST_NOT_BE_ZERO, CREATE_TRANSFER_LEDGER_MUST_NOT_BE_ZERO, CREATE_TRANSFER_CODE_MUST_NOT_BE_ZERO, CREATE_TRANSFER_DEBIT_ACCOUNT_NOT_FOUND, CREATE_TRANSFER_CREDIT_ACCOUNT_NOT_FOUND, CREATE_TRANSFER_ACCOUNTS_MUST_HAVE_THE_SAME_LEDGER, CREATE_TRANSFER_TRANSFER_MUST_HAVE_THE_SAME_LEDGER_AS_ACCOUNTS, CREATE_TRANSFER_PENDING_TRANSFER_NOT_FOUND, CREATE_TRANSFER_PENDING_TRANSFER_NOT_PENDING, CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_DEBIT_ACCOUNT_ID, CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_CREDIT_ACCOUNT_ID, CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_LEDGER, CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_CODE, CREATE_TRANSFER_EXCEEDS_PENDING_TRANSFER_AMOUNT, CREATE_TRANSFER_PENDING_TRANSFER_HAS_DIFFERENT_AMOUNT, CREATE_TRANSFER_PENDING_TRANSFER_ALREADY_POSTED, CREATE_TRANSFER_PENDING_TRANSFER_ALREADY_VOIDED, CREATE_TRANSFER_PENDING_TRANSFER_EXPIRED, CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_MUST_NOT_REGRESS, CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_MUST_POSTDATE_DEBIT_ACCOUNT, CREATE_TRANSFER_IMPORTED_EVENT_TIMESTAMP_MUST_POSTDATE_CREDIT_ACCOUNT, CREATE_TRANSFER_IMPORTED_EVENT_TIMEOUT_MUST_BE_ZERO, CREATE_TRANSFER_DEBIT_ACCOUNT_ALREADY_CLOSED, CREATE_TRANSFER_CREDIT_ACCOUNT_ALREADY_CLOSED, CREATE_TRANSFER_OVERFLOWS_DEBITS_PENDING, CREATE_TRANSFER_OVERFLOWS_CREDITS_PENDING, CREATE_TRANSFER_OVERFLOWS_DEBITS_POSTED, CREATE_TRANSFER_OVERFLOWS_CREDITS_POSTED, CREATE_TRANSFER_OVERFLOWS_DEBITS, CREATE_TRANSFER_OVERFLOWS_CREDITS, CREATE_TRANSFER_OVERFLOWS_TIMEOUT, CREATE_TRANSFER_EXCEEDS_CREDITS, CREATE_TRANSFER_EXCEEDS_DEBITS #-}

data CreateAccountsResult = CreateAccountsResult
  { index :: {-# UNPACK #-} !Word32,
    result :: {-# UNPACK #-} !Word32
  }
  deriving (Show, Read, Eq, Ord)

instance Storable CreateAccountsResult where
  sizeOf _ = (#size tb_create_accounts_result_t)
  alignment _ = (#alignment tb_create_accounts_result_t)
  peek ptr = do
    index <- (#peek tb_create_accounts_result_t, index) ptr
    result <- (#peek tb_create_accounts_result_t, result) ptr
    return CreateAccountsResult {..}
  poke ptr CreateAccountsResult {..} = do
    (#poke tb_create_accounts_result_t, index) ptr index
    (#poke tb_create_accounts_result_t, result) ptr result

data CreateTransfersResult = CreateTransfersResult
  { index :: {-# UNPACK #-} !Word32,
    result :: {-# UNPACK #-} !Word32
  }
  deriving (Show, Read, Eq, Ord)

instance Storable CreateTransfersResult where
  sizeOf _ = (#size tb_create_transfers_result_t)
  alignment _ = (#alignment tb_create_transfers_result_t)
  peek ptr = do
    index <- (#peek tb_create_transfers_result_t, index) ptr
    result <- (#peek tb_create_transfers_result_t, result) ptr
    return CreateTransfersResult {..}
  poke ptr CreateTransfersResult {..} = do
    (#poke tb_create_transfers_result_t, index) ptr index
    (#poke tb_create_transfers_result_t, result) ptr result

data AccountFilter = AccountFilter
  { account_id :: {-# UNPACK #-} !Word128,
    user_data_128 :: {-# UNPACK #-} !Word128,
    user_data_64 :: {-# UNPACK #-} !Word64,
    user_data_32 :: {-# UNPACK #-} !Word32,
    code :: {-# UNPACK #-} !Word16,
    timestamp_min :: {-# UNPACK #-} !Word64,
    timestamp_max :: {-# UNPACK #-} !Word64,
    limit :: {-# UNPACK #-} !Word32,
    flags :: {-# UNPACK #-} !Word32
  }
  deriving (Show, Read, Eq, Ord)

instance Storable AccountFilter where
  sizeOf _ = (#size tb_account_filter_t)
  alignment _ = (#alignment tb_account_filter_t)
  peek ptr = do
    account_id <- (#peek tb_account_filter_t, account_id) ptr
    user_data_128 <- (#peek tb_account_filter_t, user_data_128) ptr
    user_data_64 <- (#peek tb_account_filter_t, user_data_64) ptr
    user_data_32 <- (#peek tb_account_filter_t, user_data_32) ptr
    code <- (#peek tb_account_filter_t, code) ptr
    timestamp_min <- (#peek tb_account_filter_t, timestamp_min) ptr
    timestamp_max <- (#peek tb_account_filter_t, timestamp_max) ptr
    limit <- (#peek tb_account_filter_t, limit) ptr
    flags <- (#peek tb_account_filter_t, flags) ptr
    return AccountFilter { .. }
  poke ptr AccountFilter {..} = do
    (#poke tb_account_filter_t, account_id) ptr account_id
    (#poke tb_account_filter_t, user_data_128) ptr user_data_128
    (#poke tb_account_filter_t, user_data_64) ptr user_data_64
    (#poke tb_account_filter_t, user_data_32) ptr user_data_32
    (#poke tb_account_filter_t, code) ptr code
    pokeArray (ptr `plusPtr` (#offset tb_account_filter_t, reserved)) (replicate 58 (0 :: Word8))
    (#poke tb_account_filter_t, timestamp_min) ptr timestamp_min
    (#poke tb_account_filter_t, timestamp_max) ptr timestamp_max
    (#poke tb_account_filter_t, limit) ptr limit
    (#poke tb_account_filter_t, flags) ptr flags

newtype AccountFilterFlags = AccountFilterFlags Word32 deriving (Eq, Ord, Show, Read, Num, Enum, Real, Integral, Storable, Prim, Bits)

pattern ACCOUNT_FILTER_DEBITS = (#const TB_ACCOUNT_FILTER_DEBITS) :: AccountFilterFlags
pattern ACCOUNT_FILTER_CREDITS = (#const TB_ACCOUNT_FILTER_CREDITS) :: AccountFilterFlags
pattern ACCOUNT_FILTER_REVERSED = (#const TB_ACCOUNT_FILTER_REVERSED) :: AccountFilterFlags

{-# complete ACCOUNT_FILTER_DEBITS, ACCOUNT_FILTER_CREDITS, ACCOUNT_FILTER_REVERSED #-}

data AccountBalance = AccountBalance
  { debits_pending :: {-# UNPACK #-} !Word128,
    debits_posted :: {-# UNPACK #-} !Word128,
    credits_pending :: {-# UNPACK #-} !Word128,
    credits_posted :: {-# UNPACK #-} !Word128,
    timestamp :: {-# UNPACK #-} !Word64
  }
  deriving (Show, Read, Eq, Ord)

instance Storable AccountBalance where
  sizeOf _ = (#size tb_account_balance_t)
  alignment _ = (#alignment tb_account_balance_t)
  peek ptr = do
    debits_pending <- (#peek tb_account_balance_t, debits_pending) ptr
    debits_posted <- (#peek tb_account_balance_t, debits_posted) ptr
    credits_pending <- (#peek tb_account_balance_t, credits_pending) ptr
    credits_posted <- (#peek tb_account_balance_t, credits_posted) ptr
    timestamp <- (#peek tb_account_balance_t, timestamp) ptr
    return AccountBalance {..}
  poke ptr AccountBalance {..} = do
    (#poke tb_account_balance_t, debits_pending) ptr debits_pending
    (#poke tb_account_balance_t, debits_posted) ptr debits_posted
    (#poke tb_account_balance_t, credits_pending) ptr credits_pending
    (#poke tb_account_balance_t, credits_posted) ptr credits_posted
    (#poke tb_account_balance_t, timestamp) ptr timestamp
    pokeArray (ptr `plusPtr` (#offset tb_account_balance_t, reserved)) (replicate 56 (0 :: Word8))

data QueryFilter = QueryFilter
  { user_data_128 :: {-# UNPACK #-} !Word128,
    user_data_64 :: {-# UNPACK #-} !Word64,
    user_data_32 :: {-# UNPACK #-} !Word32,
    ledger :: {-# UNPACK #-} !Word32,
    code :: {-# UNPACK #-} !Word16,
    timestamp_min :: {-# UNPACK #-} !Word64,
    timestamp_max :: {-# UNPACK #-} !Word64,
    limit :: {-# UNPACK #-} !Word32,
    flags :: {-# UNPACK #-} !Word32
  }
  deriving (Show, Read, Eq, Ord)

instance Storable QueryFilter where
  sizeOf _ = (#size tb_query_filter_t)
  alignment _ = (#alignment tb_query_filter_t)
  peek ptr = do
    user_data_128 <- (#peek tb_query_filter_t, user_data_128) ptr
    user_data_64 <- (#peek tb_query_filter_t, user_data_64) ptr
    user_data_32 <- (#peek tb_query_filter_t, user_data_32) ptr
    ledger <- (#peek tb_query_filter_t, ledger) ptr
    code <- (#peek tb_query_filter_t, code) ptr
    timestamp_min <- (#peek tb_query_filter_t, timestamp_min) ptr
    timestamp_max <- (#peek tb_query_filter_t, timestamp_max) ptr
    limit <- (#peek tb_query_filter_t, limit) ptr
    flags <- (#peek tb_query_filter_t, flags) ptr
    return QueryFilter {..}
  poke ptr QueryFilter {..} = do
    (#poke tb_query_filter_t, user_data_128) ptr user_data_128
    (#poke tb_query_filter_t, user_data_64) ptr user_data_64
    (#poke tb_query_filter_t, user_data_32) ptr user_data_32
    (#poke tb_query_filter_t, ledger) ptr ledger
    (#poke tb_query_filter_t, code) ptr code
    pokeArray (ptr `plusPtr` (#offset tb_query_filter_t, reserved)) (replicate 6 (0 :: Word8))
    (#poke tb_query_filter_t, timestamp_min) ptr timestamp_min
    (#poke tb_query_filter_t, timestamp_max) ptr timestamp_max
    (#poke tb_query_filter_t, limit) ptr limit
    (#poke tb_query_filter_t, flags) ptr flags

newtype QueryFilterFlags = QueryFilterFlags Word32 deriving (Eq, Ord, Show, Read, Num, Enum, Real, Integral, Storable, Prim, Bits)

pattern QUERY_FILTER_REVERSED = (#const TB_QUERY_FILTER_REVERSED) :: QueryFilterFlags

{-# complete QUERY_FILTER_REVERSED #-}

data TBClient = TBClient Word64 Word64 Word64 Word64

instance Storable TBClient where
  sizeOf _ = (#size tb_client_t)
  alignment _ = (#alignment tb_client_t)
  peek ptr = do
    [a,b,c,d] <- peekArray 4 (castPtr ptr)
    return (TBClient a b c d)
  poke ptr (TBClient a b c d) = do
    pokeArray (castPtr ptr) [a,b,c,d]

type Client = Ptr TBClient

{-
  Struct containing the state of a request submitted through the client.
  This struct must be "pinned" (not copyable or movable), as its address must
  remain stable throughout the lifetime of the request.
-}
data Packet = Packet
  { user_data :: Ptr (),
    data_ptr :: Ptr (),
    data_size :: {-# UNPACK #-} !Word32,
    user_tag :: {-# UNPACK #-} !Word16,
    operation :: {-# UNPACK #-} !Word8,
    status :: {-# UNPACK #-} !Word8
  }
  deriving (Show, Eq, Ord)

instance Storable Packet where
  sizeOf _ = (#size tb_packet_t)
  alignment _ = (#alignment tb_packet_t)
  peek ptr = do
    user_data <- (#peek tb_packet_t, user_data) ptr
    data_ptr <- (#peek tb_packet_t, data) ptr
    data_size <- (#peek tb_packet_t, data_size) ptr
    user_tag <- (#peek tb_packet_t, user_tag) ptr
    operation <- (#peek tb_packet_t, operation) ptr
    status <- (#peek tb_packet_t, status) ptr
    return Packet {..}
  poke ptr Packet {..} = do
    (#poke tb_packet_t, user_data) ptr user_data
    (#poke tb_packet_t, data) ptr data_ptr
    (#poke tb_packet_t, data_size) ptr data_size
    (#poke tb_packet_t, user_tag) ptr user_tag
    (#poke tb_packet_t, operation) ptr operation
    (#poke tb_packet_t, status) ptr status

newtype Operation = Operation Word8 deriving (Eq, Ord, Show, Read, Num, Enum, Real, Integral, Storable, Prim, Bits)

pattern OPERATION_PULSE = (#const TB_OPERATION_PULSE) :: Operation
pattern OPERATION_CREATE_ACCOUNTS = (#const TB_OPERATION_CREATE_ACCOUNTS) :: Operation
pattern OPERATION_CREATE_TRANSFERS = (#const TB_OPERATION_CREATE_TRANSFERS) :: Operation
pattern OPERATION_LOOKUP_ACCOUNTS = (#const TB_OPERATION_LOOKUP_ACCOUNTS) :: Operation
pattern OPERATION_LOOKUP_TRANSFERS = (#const TB_OPERATION_LOOKUP_TRANSFERS) :: Operation
pattern OPERATION_GET_ACCOUNT_TRANSFERS = (#const TB_OPERATION_GET_ACCOUNT_TRANSFERS) :: Operation
pattern OPERATION_GET_ACCOUNT_BALANCES = (#const TB_OPERATION_GET_ACCOUNT_BALANCES) :: Operation
pattern OPERATION_QUERY_ACCOUNTS = (#const TB_OPERATION_QUERY_ACCOUNTS) :: Operation
pattern OPERATION_QUERY_TRANSFERS = (#const TB_OPERATION_QUERY_TRANSFERS) :: Operation
pattern OPERATION_GET_EVENTS = (#const TB_OPERATION_GET_EVENTS) :: Operation

{-# complete OPERATION_PULSE, OPERATION_CREATE_ACCOUNTS, OPERATION_CREATE_TRANSFERS, OPERATION_LOOKUP_ACCOUNTS, OPERATION_LOOKUP_TRANSFERS, OPERATION_GET_ACCOUNT_TRANSFERS, OPERATION_GET_ACCOUNT_BALANCES, OPERATION_QUERY_ACCOUNTS, OPERATION_QUERY_TRANSFERS, OPERATION_GET_EVENTS #-}

newtype PacketStatus = PacketStatus Word8 deriving (Eq, Ord, Show, Read, Num, Enum, Real, Integral, Storable, Prim, Bits)

pattern PACKET_OK = (#const TB_PACKET_OK) :: PacketStatus
pattern PACKET_TOO_MUCH_DATA = (#const TB_PACKET_TOO_MUCH_DATA) :: PacketStatus
pattern PACKET_CLIENT_EVICTED = (#const TB_PACKET_CLIENT_EVICTED) :: PacketStatus
pattern PACKET_CLIENT_RELEASE_TOO_LOW = (#const TB_PACKET_CLIENT_RELEASE_TOO_LOW) :: PacketStatus
pattern PACKET_CLIENT_RELEASE_TOO_HIGH = (#const TB_PACKET_CLIENT_RELEASE_TOO_HIGH) :: PacketStatus
pattern PACKET_CLIENT_SHUTDOWN = (#const TB_PACKET_CLIENT_SHUTDOWN) :: PacketStatus
pattern PACKET_INVALID_OPERATION = (#const TB_PACKET_INVALID_OPERATION) :: PacketStatus
pattern PACKET_INVALID_DATA_SIZE = (#const TB_PACKET_INVALID_DATA_SIZE) :: PacketStatus

{-# complete PACKET_OK, PACKET_TOO_MUCH_DATA, PACKET_CLIENT_EVICTED, PACKET_CLIENT_RELEASE_TOO_LOW, PACKET_CLIENT_RELEASE_TOO_HIGH, PACKET_CLIENT_SHUTDOWN, PACKET_INVALID_OPERATION, PACKET_INVALID_DATA_SIZE #-}

newtype InitStatus = InitStatus CInt deriving (Eq, Ord, Show, Read, Num, Enum, Real, Integral, Storable, Prim, Bits)

pattern INIT_SUCCESS = (#const TB_INIT_SUCCESS) :: InitStatus
pattern INIT_UNEXPECTED = (#const TB_INIT_UNEXPECTED) :: InitStatus
pattern INIT_OUT_OF_MEMORY = (#const TB_INIT_OUT_OF_MEMORY) :: InitStatus
pattern INIT_ADDRESS_INVALID = (#const TB_INIT_ADDRESS_INVALID) :: InitStatus
pattern INIT_ADDRESS_LIMIT_EXCEEDED = (#const TB_INIT_ADDRESS_LIMIT_EXCEEDED) :: InitStatus
pattern INIT_SYSTEM_RESOURCES = (#const TB_INIT_SYSTEM_RESOURCES) :: InitStatus
pattern INIT_NETWORK_SUBSYSTEM = (#const TB_INIT_NETWORK_SUBSYSTEM) :: InitStatus

{-# complete INIT_SUCCESS, INIT_UNEXPECTED, INIT_OUT_OF_MEMORY, INIT_ADDRESS_INVALID, INIT_ADDRESS_LIMIT_EXCEEDED, INIT_SYSTEM_RESOURCES, INIT_NETWORK_SUBSYSTEM #-}

newtype ClientStatus = ClientStatus CInt deriving (Eq, Ord, Show, Read, Num, Enum, Real, Integral, Storable, Prim, Bits)

pattern CLIENT_OK = (#const TB_CLIENT_OK) :: ClientStatus
pattern CLIENT_INVALID = (#const TB_CLIENT_INVALID) :: ClientStatus

{-# complete CLIENT_OK, CLIENT_INVALID #-}

newtype RegisterLogCallbackStatus = RegisterLogCallbackStatus CInt deriving (Eq, Ord, Show, Read, Num, Enum, Real, Integral, Storable, Prim, Bits)

pattern REGISTER_LOG_CALLBACK_SUCCESS = (#const TB_REGISTER_LOG_CALLBACK_SUCCESS) :: RegisterLogCallbackStatus
pattern REGISTER_LOG_CALLBACK_ALREADY_REGISTERED = (#const TB_REGISTER_LOG_CALLBACK_ALREADY_REGISTERED) :: RegisterLogCallbackStatus
pattern REGISTER_LOG_CALLBACK_NOT_REGISTERED = (#const TB_REGISTER_LOG_CALLBACK_NOT_REGISTERED) :: RegisterLogCallbackStatus

{-# complete REGISTER_LOG_CALLBACK_SUCCESS, REGISTER_LOG_CALLBACK_ALREADY_REGISTERED, REGISTER_LOG_CALLBACK_NOT_REGISTERED #-}

newtype LogLevel = LogLevel CInt deriving (Eq, Ord, Show, Read, Num, Enum, Real, Integral, Storable, Prim, Bits)

pattern LOG_ERR = (#const TB_LOG_ERR) :: LogLevel
pattern LOG_WARN = (#const TB_LOG_WARN) :: LogLevel
pattern LOG_INFO = (#const TB_LOG_INFO) :: LogLevel
pattern LOG_DEBUG = (#const TB_LOG_DEBUG) :: LogLevel

{-# complete LOG_ERR, LOG_WARN, LOG_INFO, LOG_DEBUG #-}
