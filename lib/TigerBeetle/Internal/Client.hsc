{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module DuckDB.Internal.Context (
  Database,
  Connection,
  Result,
  DString(..),
  Chunk,
  Vector,
  LogicalType,
  State(..),
  CState(..),
  fromCState,
  DbType(..),
  CDbType(..),
  fromCDbType,
  sizeOfDbType,
  duckdbCtx
) where

import qualified Data.Map as Map
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Language.C.Inline.Context
import qualified Language.C.Types as C

#include <duckdb.h>

data Database
data Connection
data Chunk
data Vector
data LogicalType

data Result
instance Storable Result where
  sizeOf _ = (#size duckdb_result)
  alignment _ = (#alignment duckdb_result)
  peek _ = error "peek not implemented for Result"
  poke _ _ = error "poke not implemented for Result"

data DString = DString
  { dstrSize :: CUInt
  , dstrString :: String
  }
  deriving Show

instance Storable DString where
  sizeOf _ = 4 + sizeOf @CUInt undefined + sizeOf @CString undefined
  alignment _ = (#alignment uint32_t)
  peek ptr = do
    len <- peek (castPtr ptr)
    sptr <- if (len > 4) then
              peek (castPtr $ plusPtr ptr 8)
            else
              pure (castPtr $ plusPtr ptr 4)
    str <- peekCStringLen (sptr, fromIntegral len)
    pure $ DString len str
  poke _ _ = error "poke not implemented for DString"

newtype CState = CState { unCState :: CInt }
  deriving (Show, Eq)

data State
  = Success
  | Error
  deriving (Show, Read, Eq, Ord)

instance Enum State where
  fromEnum Success = (#const DuckDBSuccess)
  fromEnum Error = (#const DuckDBError)

  toEnum (#const DuckDBSuccess) = Success
  toEnum (#const DuckDBError) = Error
  toEnum e = errorWithoutStackTrace ("invalid enum for State: " ++ show e)

fromCState :: CState -> State
fromCState = toEnum . fromIntegral . unCState 

newtype CDbType = CDbType { unCDbType :: CInt }
  deriving (Show, Eq)

data DbType
  = Invalid
  | Boolean       -- bool
  | TinyInt       -- int8_t
  | SmallInt      -- int16_t
  | Integer       -- int32_t
  | BigInt        -- int64_t
  | UTinyInt      -- uint8_t
  | USmallInt     -- uint16_t
  | UInteger      -- uint32_t
  | UBigInt       -- uint64_t
  | Float         -- float
  | Double        -- double
  | Timestamp     -- duckdb_timestamp, in microseconds
  | Date          -- duckdb_date
  | Time          -- duckdb_time
  | Interval      -- duckdb_interval
  | HugeInt       -- duckdb_hugeint
  | VarChar       -- const char*
  | Blob          -- duckdb_blob
  | Decimal       -- decimal
  | TimestampS    -- duckdb_timestamp, in seconds
  | TimestampMS   -- duckdb_timestamp, in milliseconds
  | TimestampNS   -- duckdb_timestamp, in nanoseconds
  | Enum          -- enum type, only useful as logical type
  | List          -- list type, only useful as logical type
  | Struct        -- struct type, only useful as logical type
  | Map           -- map type, only useful as logical type
  | Uuid          -- duckdb_hugeint
  | Json          -- const char*
  deriving (Show, Read, Eq, Ord)

instance Enum DbType where
  fromEnum Invalid = (#const DUCKDB_TYPE_INVALID)
  fromEnum Boolean = (#const DUCKDB_TYPE_BOOLEAN)
  fromEnum TinyInt = (#const DUCKDB_TYPE_TINYINT)
  fromEnum SmallInt = (#const DUCKDB_TYPE_SMALLINT)
  fromEnum Integer = (#const DUCKDB_TYPE_INTEGER)
  fromEnum BigInt = (#const DUCKDB_TYPE_BIGINT)
  fromEnum UTinyInt = (#const DUCKDB_TYPE_UTINYINT)
  fromEnum USmallInt = (#const DUCKDB_TYPE_USMALLINT)
  fromEnum UInteger = (#const DUCKDB_TYPE_UINTEGER)
  fromEnum UBigInt = (#const DUCKDB_TYPE_UBIGINT)
  fromEnum Float = (#const DUCKDB_TYPE_FLOAT)
  fromEnum Double = (#const DUCKDB_TYPE_DOUBLE)
  fromEnum Timestamp = (#const DUCKDB_TYPE_TIMESTAMP)
  fromEnum Date = (#const DUCKDB_TYPE_DATE)
  fromEnum Time = (#const DUCKDB_TYPE_TIME)
  fromEnum Interval = (#const DUCKDB_TYPE_INTERVAL)
  fromEnum HugeInt = (#const DUCKDB_TYPE_HUGEINT)
  fromEnum VarChar = (#const DUCKDB_TYPE_VARCHAR)
  fromEnum Blob = (#const DUCKDB_TYPE_BLOB)
  fromEnum Decimal = (#const DUCKDB_TYPE_DECIMAL)
  fromEnum TimestampS = (#const DUCKDB_TYPE_TIMESTAMP_S)
  fromEnum TimestampMS = (#const DUCKDB_TYPE_TIMESTAMP_MS)
  fromEnum TimestampNS = (#const DUCKDB_TYPE_TIMESTAMP_NS)
  fromEnum Enum = (#const DUCKDB_TYPE_ENUM)
  fromEnum List = (#const DUCKDB_TYPE_LIST)
  fromEnum Struct = (#const DUCKDB_TYPE_STRUCT)
  fromEnum Map = (#const DUCKDB_TYPE_MAP)
  fromEnum Uuid = (#const DUCKDB_TYPE_UUID)
  fromEnum Json = (#const DUCKDB_TYPE_JSON)

  toEnum (#const DUCKDB_TYPE_INVALID) = Invalid
  toEnum (#const DUCKDB_TYPE_BOOLEAN) = Boolean
  toEnum (#const DUCKDB_TYPE_TINYINT) = TinyInt
  toEnum (#const DUCKDB_TYPE_SMALLINT) = SmallInt
  toEnum (#const DUCKDB_TYPE_INTEGER) = Integer
  toEnum (#const DUCKDB_TYPE_BIGINT) = BigInt
  toEnum (#const DUCKDB_TYPE_UTINYINT) = UTinyInt
  toEnum (#const DUCKDB_TYPE_USMALLINT) = USmallInt
  toEnum (#const DUCKDB_TYPE_UINTEGER) = UInteger
  toEnum (#const DUCKDB_TYPE_UBIGINT) = UBigInt
  toEnum (#const DUCKDB_TYPE_FLOAT) = Float
  toEnum (#const DUCKDB_TYPE_DOUBLE) = Double
  toEnum (#const DUCKDB_TYPE_TIMESTAMP) = Timestamp
  toEnum (#const DUCKDB_TYPE_DATE) = Date
  toEnum (#const DUCKDB_TYPE_TIME) = Time
  toEnum (#const DUCKDB_TYPE_INTERVAL) = Interval
  toEnum (#const DUCKDB_TYPE_HUGEINT) = HugeInt
  toEnum (#const DUCKDB_TYPE_VARCHAR) = VarChar
  toEnum (#const DUCKDB_TYPE_BLOB) = Blob
  toEnum (#const DUCKDB_TYPE_DECIMAL) = Decimal
  toEnum (#const DUCKDB_TYPE_TIMESTAMP_S) = TimestampS
  toEnum (#const DUCKDB_TYPE_TIMESTAMP_MS) = TimestampMS
  toEnum (#const DUCKDB_TYPE_TIMESTAMP_NS) = TimestampNS
  toEnum (#const DUCKDB_TYPE_ENUM) = Enum
  toEnum (#const DUCKDB_TYPE_LIST) = List
  toEnum (#const DUCKDB_TYPE_STRUCT) = Struct
  toEnum (#const DUCKDB_TYPE_MAP) = Map
  toEnum (#const DUCKDB_TYPE_UUID) = Uuid
  toEnum (#const DUCKDB_TYPE_JSON) = Json
  toEnum e = errorWithoutStackTrace ("invalid enum for DbType: " ++ show e)

fromCDbType :: CDbType -> DbType
fromCDbType = toEnum . fromIntegral . unCDbType 

sizeOfDbType :: DbType -> CSize
sizeOfDbType Invalid = 0
sizeOfDbType Boolean = (#size bool)
sizeOfDbType TinyInt = (#size int8_t)
sizeOfDbType SmallInt = (#size int16_t)
sizeOfDbType Integer = (#size int32_t)
sizeOfDbType BigInt = (#size int64_t)
sizeOfDbType UTinyInt = (#size uint8_t)
sizeOfDbType USmallInt = (#size uint16_t)
sizeOfDbType UInteger = (#size uint32_t)
sizeOfDbType UBigInt = (#size uint64_t)
sizeOfDbType Float = (#size float)
sizeOfDbType Double = (#size double)
sizeOfDbType Timestamp = (#size duckdb_timestamp)
sizeOfDbType Date = (#size duckdb_date)
sizeOfDbType Time = (#size duckdb_time)
sizeOfDbType Interval = (#size duckdb_interval)
sizeOfDbType HugeInt = (#size duckdb_hugeint)
sizeOfDbType VarChar = (#size const char*)
sizeOfDbType Blob = (#size duckdb_blob)
sizeOfDbType Decimal = (#size duckdb_decimal)
sizeOfDbType TimestampS = (#size duckdb_timestamp)
sizeOfDbType TimestampMS = (#size duckdb_timestamp)
sizeOfDbType TimestampNS = (#size duckdb_timestamp)
sizeOfDbType Enum = 0
sizeOfDbType List = 0
sizeOfDbType Struct = 0
sizeOfDbType Map = 0
sizeOfDbType Uuid = (#size duckdb_hugeint)
sizeOfDbType Json = 0

duckdbCtx :: Context
duckdbCtx = baseCtx <> ctx
 where
  ctx =
    mempty
      { ctxTypesTable = duckdbTypesTable
      }

duckdbTypesTable :: TypesTable
duckdbTypesTable =
  Map.fromList
    [ (C.TypeName "duckdb_state", [t| CState |])
    , (C.TypeName "duckdb_database", [t| Ptr Database |])
    , (C.TypeName "duckdb_connection", [t| Ptr Connection |])
    , (C.TypeName "duckdb_result", [t| Result |])
    , (C.TypeName "duckdb_data_chunk", [t| Ptr Chunk |])
    , (C.TypeName "duckdb_vector", [t| Ptr Vector |])
    , (C.TypeName "duckdb_logical_type", [t| Ptr LogicalType |])
    , (C.TypeName "duckdb_type", [t| CDbType |])
    , (C.TypeName "duckdb_String", [t| DString |])
    , (C.TypeName "idx_t", [t| CULong |])
    ]
