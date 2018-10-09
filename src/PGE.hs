{-# LANGUAGE OverloadedStrings #-}

module PGE where

import Data.Time
import Data.List (intercalate)
import Data.Text (Text, unpack, pack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC

tsToUnix :: TimeZone -> String -> Text -> Maybe Text
tsToUnix tz fmt s =
  let tl = defaultTimeLocale in
    pack <$> formatTime tl "%s000000000" <$> localTimeToUTC tz <$> parseTimeM True tl fmt (unpack s)


encode :: [Text] -> BL.ByteString
encode rows = BL.fromStrict $ BC.pack $ intercalate "\n" $ (map unpack) rows
