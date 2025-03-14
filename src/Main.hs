{-# LANGUAGE OverloadedStrings #-}

module Main where

import TigerBeetle

main :: IO ()
main = do
  -- withClient 1 "127.0.0.1:10000" $ \client -> do
  -- putStrLn "Client created successfully"
  -- result <- pulse client
  -- putStrLn $ "Result: " ++ show result
  putStrLn "Hello, World!"
