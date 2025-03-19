{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module TigerBeetle.Internal.Context where

import qualified Data.Map as Map
import Language.C.Inline
import Language.C.Inline.Context
import Language.C.Types
import TigerBeetle.Internal.Client

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
    [ (TypeName "TB_ACCOUNT_FLAGS", [t|AccountFlags|])
    , (TypeName "tb_account_t", [t|Account|])
    , (TypeName "TB_TRANSFER_FLAGS", [t|TransferFlags|])
    , (TypeName "tb_transfer_t", [t|Transfer|])
    , (TypeName "TB_CREATE_ACCOUNT_RESULT", [t|CreateAccountResult|])
    , (TypeName "TB_CREATE_TRANSFER_RESULT", [t|CreateTransferResult|])
    , (TypeName "TB_CREATE_ACCOUNTS_RESULT", [t|CreateAccountsResult|])
    , (TypeName "TB_CREATE_TRANSFERS_RESULT", [t|CreateTransfersResult|])
    , (TypeName "tb_account_filter_t", [t|AccountFilter|])
    , (TypeName "TB_ACCOUNT_FILTER_FLAGS", [t|AccountFilterFlags|])
    , (TypeName "tb_account_balance_t", [t|AccountBalance|])
    , (TypeName "tb_query_filter_t", [t|QueryFilter|])
    , (TypeName "TB_QUERY_FILTER_FLAGS", [t|QueryFilterFlags|])
    , (TypeName "tb_client_t", [t|TBClient|])
    , (TypeName "tb_packet_t", [t|Packet|])
    , (TypeName "TB_OPERATION", [t|Operation|])
    , (TypeName "TB_PACKET_STATUS", [t|PacketStatus|])
    , (TypeName "TB_INIT_STATUS", [t|InitStatus|])
    , (TypeName "TB_CLIENT_STATUS", [t|ClientStatus|])
    , (TypeName "TB_REGISTER_LOG_CALLBACK_STATUS", [t|RegisterLogCallbackStatus|])
    , (TypeName "TB_LOG_LEVEL", [t|LogLevel|])
    ]