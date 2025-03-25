{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import TigerBeetle

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "TigerBeetle Client Tests"
    [ testGroup
        "Unit Tests"
        [ testCase "Create one account" $ do
            withTestClient $ \client -> do
              let account1 = Account 1 0 0 0 0 0 0 0 1 1 0 0
              result <- echoCreateAccounts client [account1]
              assertEqual
                "Create accounts should succeed"
                [account1]
                result
        ]
    ]

withTestClient :: (Client -> IO a) -> IO a
withTestClient = withEchoClient 0 "127.0.0.1:3001"