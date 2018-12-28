{-# LANGUAGE OverloadedStrings #-}

module PGE where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Csv             (HasHeader (..), decode)
import           Data.Text            (Text, pack, unpack)
import           Data.Time
import qualified Data.Vector          as V
import           System.IO            (Handle)

tsToUnix :: TimeZone -> String -> Text -> Maybe Text
tsToUnix tz fmt s =
  let tl = defaultTimeLocale in
    pack <$> formatTime tl "%s000000000" <$> localTimeToUTC tz <$> parseTimeM True tl fmt (unpack s)

type FilterFun = (V.Vector Text -> Bool)

type RewriteFun = (Text -> Maybe Text) -> V.Vector Text -> [Text]

parseCSV :: Handle -> IO (Either String (V.Vector (V.Vector Text)))
parseCSV h = decode NoHeader <$> BL.hGetContents h

parseCSV' :: B.ByteString -> Either String (V.Vector (V.Vector Text))
parseCSV' = decode NoHeader . BL.fromStrict

mkTimeParser :: String -> IO (Text -> Maybe UTCTime)
mkTimeParser fmt = do
  tz <- getCurrentTimeZone
  pure $ \s -> localTimeToUTC tz <$> parseTimeM True defaultTimeLocale fmt (unpack s)
