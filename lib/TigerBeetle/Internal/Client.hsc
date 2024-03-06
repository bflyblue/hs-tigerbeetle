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

module TigerBeetle.Internal.Client
( Account(..)
, AccountFlags
  ( ACCOUNT_LINKED
  , ACCOUNT_DEBITS_MUST_NOT_EXCEED_CREDITS
  , ACCOUNT_CREDITS_MUST_NOT_EXCEED_DEBITS
  , ACCOUNT_HISTORY)
, Transfer(..)
, TransferFlags
  ( TRANSFER_LINKED
  , TRANSFER_PENDING
  , TRANSFER_POST_PENDING_TRANSFER
  , TRANSFER_VOID_PENDING_TRANSFER
  , TRANSFER_BALANCING_DEBIT
  , TRANSFER_BALANCING_CREDIT
  )
) where

import Data.Bits
import qualified Data.Map as Map
import Data.Primitive.Types
import Data.WideWord
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import qualified Language.C.Inline as C
import Language.C.Inline.Context
import qualified Language.C.Types as C

#include <tb_client.h>

newtype AccountFlags = AccountFlags Word16 deriving (Eq, Ord, Show, Read, Num, Enum, Real, Integral, Storable, Prim, Bits)

pattern ACCOUNT_LINKED = (#const TB_ACCOUNT_LINKED) :: AccountFlags
pattern ACCOUNT_DEBITS_MUST_NOT_EXCEED_CREDITS = (#const TB_ACCOUNT_DEBITS_MUST_NOT_EXCEED_CREDITS) :: AccountFlags
pattern ACCOUNT_CREDITS_MUST_NOT_EXCEED_DEBITS = (#const TB_ACCOUNT_CREDITS_MUST_NOT_EXCEED_DEBITS) :: AccountFlags
pattern ACCOUNT_HISTORY = (#const TB_ACCOUNT_HISTORY) :: AccountFlags

data Account = Account
  { id :: {-# UNPACK #-} !Word128,
    debits_pending :: {-# UNPACK #-} !Word128,
    debits_posted :: {-# UNPACK #-} !Word128,
    credits_pending :: {-# UNPACK #-} !Word128,
    credits_posted :: {-# UNPACK #-} !Word128,
    user_data_128 :: {-# UNPACK #-} !Word128,
    user_data_64 :: {-# UNPACK #-} !Word64,
    user_data_32 :: {-# UNPACK #-} !Word32,
    reserved :: {-# UNPACK #-} !Word32,
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
    reserved <- (#peek tb_account_t, reserved) ptr
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
    (#poke tb_account_t, reserved) ptr reserved
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

{-# complete TRANSFER_LINKED, TRANSFER_PENDING, TRANSFER_POST_PENDING_TRANSFER, TRANSFER_VOID_PENDING_TRANSFER, TRANSFER_BALANCING_DEBIT, TRANSFER_BALANCING_CREDIT #-}

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

tigerbeetleCtx :: Context
tigerbeetleCtx = baseCtx <> ctx
 where
  ctx =
    mempty
      { ctxTypesTable = tigerbeetleTypesTable
      }

tigerbeetleTypesTable :: TypesTable
tigerbeetleTypesTable =
  Map.fromList
    [ (C.TypeName "tb_account_t", [t| Account |])
    ]