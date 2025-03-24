module TigerBeetle.Internal.Timestamp where

import Data.Bits
import Data.Coerce (coerce)
import Data.Fixed (Fixed (MkFixed), Nano, Pico)
import Data.Primitive.Types (Prim)
import Data.String (IsString (fromString))
import Data.Time (UTCTime, secondsToNominalDiffTime)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.Word (Word64)
import Foreign.C (CTime)
import Foreign.Storable (Storable)

-- | A timestamp in nanoseconds since the epoch.
newtype Timestamp = Timestamp Word64 deriving (Eq, Ord, Read, Num, Real, Storable, Prim, Bits)

instance IsString Timestamp where
  fromString str =
    case fromISO8601 str of
      Just ts -> ts
      Nothing -> error $ "Invalid timestamp: " ++ str

instance Show Timestamp where
  showsPrec p t = showParen (p > 10) $ showString $ "\"" <> toISO8601 t <> "\""

fromCTime :: CTime -> Timestamp
fromCTime = fromPOSIXTime . realToFrac

picoToNano :: Pico -> Nano
picoToNano (MkFixed pico) = MkFixed (pico `div` 1000)

nanoToPico :: Nano -> Pico
nanoToPico (MkFixed nano) = MkFixed (nano * 1000)

fromPOSIXTime :: POSIXTime -> Timestamp
fromPOSIXTime = fromInteger . coerce . picoToNano . nominalDiffTimeToSeconds

fromUTCTime :: UTCTime -> Timestamp
fromUTCTime = fromPOSIXTime . utcTimeToPOSIXSeconds

toUTCTime :: Timestamp -> UTCTime
toUTCTime (Timestamp w) =
  let nano :: Nano = MkFixed $ fromIntegral w
   in posixSecondsToUTCTime $ secondsToNominalDiffTime $ nanoToPico nano

fromISO8601 :: String -> Maybe Timestamp
fromISO8601 = fmap fromUTCTime . iso8601ParseM

toISO8601 :: Timestamp -> String
toISO8601 = iso8601Show . toUTCTime
