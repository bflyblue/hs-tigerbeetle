{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Bits
import TigerBeetle
import TigerBeetle.Internal.Client

main :: IO ()
main = do
  let account1 = Account 1 0 0 0 0 0 0 0 1 1 0 0
      account2 = Account 2 0 0 0 0 0 0 0 1 1 0 0
      filter1 = AccountFilter 1 0 0 0 0 0 0 10 (ACCOUNT_FILTER_DEBITS .|. ACCOUNT_FILTER_CREDITS)
      filter2 = AccountFilter 2 0 0 0 0 0 0 10 (ACCOUNT_FILTER_DEBITS .|. ACCOUNT_FILTER_CREDITS)
      transfer = Transfer 1 1 2 100 0 0 0 0 0 1 1 0 0
  withClient 0 "127.0.0.1:3001" $ \client -> do
    putStrLn "Client created successfully"
    result1 <- createAccounts client [account1, account2]
    putStrLn $ "Result: " ++ show result1
    result2 <- lookupAccounts client [1, 2]
    putStrLn $ "Result: " ++ show result2
    result3 <- createTransfers client [transfer]
    putStrLn $ "Result: " ++ show result3
    result4 <- getAccountTransfers client filter1
    putStrLn $ "Result: " ++ show result4
    result5 <- lookupTransfers client [1]
    putStrLn $ "Result: " ++ show result5