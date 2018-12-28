{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module DB (
  EnergyType(..), Site(..), Lasts(..),
  lastTimestamps, lastTimestamp,
  toLocal,
  myQueryParams, myWriteParams,
  findQueryHost,
  QueryParams, WriteParams
) where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (race)
import           Control.Exception        (SomeException (..), catch)
import           Control.Lens
import           Control.Monad            (when)
import           Data.Text                (Text, pack, toLower, unpack)
import           Data.Time
import qualified Data.Vector              as V
import           Data.Void                (Void)
import           Database.InfluxDB
import           Database.InfluxDB.Format (field)
import           Database.InfluxDB.Types  (Nullability (..))

data EnergyType = Electric | Gas | Solar deriving (Eq, Show)

energy_type :: Text -> EnergyType
energy_type "electric" = Electric
energy_type "gas"      = Gas
energy_type "solar"    = Solar
energy_type x          = error ("unknown energy type: " <> unpack x)

data Site = SJ | Oro deriving (Show)

site :: Text -> Site
site "oro" = Oro
site "sj"  = SJ
site x     = error ("unknown site: " <> unpack x)

data Lasts = Lasts UTCTime EnergyType Site deriving (Show)

instance QueryResults Lasts where
  parseResults prec = parseResultsWithDecoder strictDecoder $ \_ m columns fields -> do
    time <- getField "time" columns fields >>= parseUTCTime prec
    let Just et = energy_type <$> m ^.at "energy_type"
        Just s  = site <$> m ^.at "site"
    pure $ Lasts time et s


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
    toField = FieldString . toLower . pack . show

toLocal :: UTCTime -> IO LocalTime
toLocal ts = do
  tz <- getCurrentTimeZone
  pure $ utcToLocalTime tz ts

findQueryHost :: IO Server
findQueryHost = do
  esn <- race (tryHost "localhost") (tryHost "eve")
  let sn = case esn of
             Left _  -> "localhost"
             Right _ -> "eve"
  let qp = queryParams "pge" ^. server
  pure (qp & host .~ sn)

  where
    tryHost :: Text -> IO ()
    tryHost h = do
      let p = queryParams "pge" & server.host .~ h
      _ <- catch (query p "show databases" :: IO (V.Vector Void)) (\e -> threadDelay 100000 >> mempty
                                                                    (e :: SomeException))
      pure ()

myQueryParams :: IO QueryParams
myQueryParams = do
  h <- findQueryHost
  pure $ queryParams "pge" & server .~ h

myWriteParams :: IO WriteParams
myWriteParams = do
  h <- findQueryHost
  pure $ writeParams "pge" & server .~ h & precision .~ Minute
