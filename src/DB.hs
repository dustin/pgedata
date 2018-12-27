{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module DB (
  EnergyType(..), Site(..), Lasts(..),
  lastTimestamps, lastTimestamp,
  toLocal,
  queryParams
) where

import           Control.Lens
import           Control.Monad            (when)
import           Data.Char                (toLower)
import           Data.String              (fromString)
import           Data.Text                (Text, unpack)
import           Data.Time
import qualified Data.Vector              as V
import           Database.InfluxDB
import           Database.InfluxDB.Format (field)
import           Database.InfluxDB.Types  (Nullability (..))

data EnergyType = Electric | Gas | Solar deriving (Eq, Show)

data Site = SJ | Oro deriving (Show)

data Lasts = Lasts UTCTime EnergyType Site deriving (Show)

instance QueryResults Lasts where
  parseResults prec = parseResultsWithDecoder strictDecoder $ \_ m columns fields -> do
    time <- getField "time" columns fields >>= parseUTCTime prec
    let Just et = energy_type <$> m ^.at "energy_type"
        Just s  = site <$> m ^.at "site"
    pure $ Lasts time et s

      where
        site :: Text -> Site
        site "oro" = Oro
        site "sj"  = SJ
        site x     = error ("unknown site: " <> unpack x)

        energy_type :: Text -> EnergyType
        energy_type "electric" = Electric
        energy_type "gas"      = Gas
        energy_type "solar"    = Solar
        energy_type x          = error ("unknown energy type: " <> unpack x)


lastTimestamps :: QueryParams -> IO (V.Vector Lasts)
lastTimestamps p =
  query p "select last(value) from energy group by site, energy_type" :: IO (V.Vector Lasts)

newtype TSOnly = TSOnly UTCTime

instance QueryResults TSOnly where
  parseResults prec = parseResultsWithDecoder strictDecoder $ \_ _ columns fields ->
    TSOnly <$> (getField "time" columns fields >>= parseUTCTime prec)


lastTimestamp :: QueryParams -> EnergyType -> Site -> IO UTCTime
lastTimestamp p et s = do
  let etf = toField et
      sf = toField s
  let q = formatQuery ("select last(value) from energy where energy_type=" %field% " and site=" %field) etf sf

  r <- query p q :: IO (V.Vector TSOnly)

  when (null r) $ fail "no results returned"

  let (TSOnly x) = V.head r
  pure x

  where
    toField :: Show a => a -> Field 'Nullable
    toField = fromString . map toLower . show

toLocal :: UTCTime -> IO LocalTime
toLocal ts = do
  tz <- getCurrentTimeZone
  pure $ utcToLocalTime tz ts
