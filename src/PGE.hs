{-# LANGUAGE OverloadedStrings #-}

module PGE where

import           Control.Monad        (mapM_)
import qualified Data.ByteString.Lazy as BL
import           Data.Csv             (HasHeader (..), decode)
import           Data.Text            (Text, pack, unpack)
import qualified Data.Text.IO         as TIO
import           Data.Time
import qualified Data.Vector          as V
import           System.IO            (Handle, stdin)

tsToUnix :: TimeZone -> String -> Text -> Maybe Text
tsToUnix tz fmt s =
  let tl = defaultTimeLocale in
    pack <$> formatTime tl "%s000000000" <$> localTimeToUTC tz <$> parseTimeM True tl fmt (unpack s)

type FilterFun = (V.Vector Text -> Bool)

type RewriteFun = (Text -> Maybe Text) -> V.Vector Text -> [Text]

parseCSV :: Handle -> IO (Either String (V.Vector (V.Vector Text)))
parseCSV h = decode NoHeader <$> BL.hGetContents h

mkTimeParser :: String -> IO (Text -> Maybe UTCTime)
mkTimeParser fmt = do
  tz <- getCurrentTimeZone
  pure $ \s -> localTimeToUTC tz <$> parseTimeM True defaultTimeLocale fmt (unpack s)

process' :: Handle -> String -> FilterFun -> RewriteFun -> IO ()
process' h tfmt ff rf = do
  tz <- getCurrentTimeZone
  let tparse = tsToUnix tz tfmt
  csvd <- parseCSV h
  case csvd of
    Left err -> fail (show err)
    Right v -> mapM_ TIO.putStrLn $ (concatMap (rf tparse) . filter ff) (V.toList v)

process :: String -> FilterFun -> RewriteFun -> IO ()
process = process' stdin

