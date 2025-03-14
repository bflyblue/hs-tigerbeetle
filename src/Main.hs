{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import TigerBeetle

main :: IO ()
main = do
  withClient 0 "127.0.0.1:3001" $ \client -> do
    putStrLn "Client created successfully"
    result <- pulse client
    putStrLn $ "Result: " ++ show result
    threadDelay 1000000
