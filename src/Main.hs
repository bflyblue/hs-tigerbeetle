{-# LANGUAGE OverloadedStrings #-}

module Main where

import TigerBeetle

main :: IO ()
main = do
  let accounts = [Account 1 0 0 0 0 0 0 0 1 1 0 0]

  withClient 0 "127.0.0.1:3001" $ \client -> do
    putStrLn "Client created successfully"
    result <- createAccounts client accounts
    putStrLn $ "Result: " ++ show result
