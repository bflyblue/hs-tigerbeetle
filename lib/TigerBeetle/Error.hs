module TigerBeetle.Error where

import Control.Exception

newtype TigerBeetleError = TigerBeetleError String
  deriving (Show)

instance Exception TigerBeetleError
