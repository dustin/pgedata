{-# LANGUAGE OverloadedStrings #-}

module PGE where

import Data.Time
import Data.Text (Text, unpack, pack)

tsToUnix :: TimeZone -> Text -> Maybe Text
tsToUnix tz s =
  let tl = defaultTimeLocale in
    pack <$> formatTime tl "%s000000000" <$> localTimeToUTC tz <$> parseTimeM True tl "%Y-%-m-%-d %H:%M" (unpack s)
