# TigerBeetle Haskell Client

A Haskell client library for the [TigerBeetle](https://tigerbeetle.com/) accounting database. This library provides bindings to the TigerBeetle C client interface, offering a more idiomatic and type-safe Haskell API.

## About TigerBeetle

TigerBeetle is a distributed financial accounting database designed for mission-critical safety and performance. It provides:

* ACID transactions
* 1-phase and 2-phase transfers
* Double-entry accounting
* Strongly consistent replication
* High throughput and low latency

## Features

* Low-level Haskell bindings to the TigerBeetle C client interface
* Type-safe wrappers around C data structures
* Client initialization and management
* Account management (create, lookup, query)
* Transfer operations (create, lookup, query)
* Balance retrieval and account transfers
* Echo client for testing and development
* Proper Haskell error handling

## Installation

This package requires the TigerBeetle C client library to be available on your system.

Add to your `cabal.project`:

```
packages: .
source-repository-package
  type: git
  location: https://github.com/bflyblue/hs-tigerbeetle.git
  tag: v0.1.0.0
```

Or in `package.yaml` or `.cabal` file:

```
build-depends:
  , hs-tigerbeetle >= 0.1.0.0
```

## Usage Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import TigerBeetle

-- Define cluster address
clusterAddress :: ByteString
clusterAddress = "127.0.0.1:3000"

-- Define cluster ID
clusterId :: ClusterId
clusterId = 0

main :: IO ()
main = do
  -- Create a client with bracket-style resource management
  withClient clusterId clusterAddress $ \client -> do
    -- Create an account
    let account = Account 
          { id = 1                -- Account ID
          , debitsPending = 0     -- Pending debits
          , debitsPosted = 0      -- Posted debits
          , creditsPending = 0    -- Pending credits
          , creditsPosted = 0     -- Posted credits
          , timestamp = 0         -- Timestamp
          , ledger = 1            -- Ledger ID
          , code = 1              -- Account code
          , flags = 0             -- Account flags
          , reserved = 0          -- Reserved
          }
    
    result <- createAccounts client [account]
    print result
    
    -- Perform a transfer
    let transfer = Transfer
          { id = 1               -- Transfer ID
          , debitAccountId = 1   -- Debit account ID
          , creditAccountId = 2  -- Credit account ID
          , amount = 100         -- Amount
          , pendingId = 0        -- Pending ID
          , userdata = 0         -- User data
          , timeout = 0          -- Timeout
          , timestamp = 0        -- Timestamp
          , ledger = 1           -- Ledger ID
          , code = 1             -- Transfer code
          , flags = 0            -- Transfer flags
          , reserved = 0         -- Reserved
          }
    
    transferResult <- createTransfers client [transfer]
    print transferResult
```

## API Documentation

For detailed API documentation, please refer to the Haddock documentation or the source code.

Main modules:

- `TigerBeetle` - Main module with the public API
- `TigerBeetle.Error` - Error handling
- `TigerBeetle.Identifier` - Type-safe identifiers
- `TigerBeetle.Internal.*` - Low-level bindings and implementation details

## Requirements

- GHC 9.0+
- TigerBeetle C client library (`tb_client`)
- Cabal or Stack build system

## License

This library is released under the BSD-3-Clause License. See the LICENSE file for details.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## Links

- [TigerBeetle Documentation](https://docs.tigerbeetle.com/)
- [TigerBeetle GitHub](https://github.com/tigerbeetledb/tigerbeetle) 