# Revision history for tigerbeetle

## 0.1.0.0 -- 2023-03-25

* Initial release of the Haskell client for TigerBeetle accounting database
* Core features:
  * Client initialization and management
  * Account management (create/lookup/query accounts)
  * Transfer operations (create/lookup/query transfers)
  * Account balance retrieval
  * Account transfers retrieval
  * Full support for TigerBeetle data types and operations
* Low-level C FFI bindings to TigerBeetle's C client
* High-level Haskell API with proper error handling
* Echo client for testing and development
