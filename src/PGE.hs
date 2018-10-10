{-# LANGUAGE OverloadedStrings #-}

module PGE where

import Data.Time
import Control.Monad (mapM_)
import Data.List (intercalate)
import Data.Text (Text, unpack, pack)
import qualified Data.Text.IO as TIO

tsToUnix :: TimeZone -> String -> Text -> Maybe Text
tsToUnix tz fmt s =
  let tl = defaultTimeLocale in
    pack <$> formatTime tl "%s000000000" <$> localTimeToUTC tz <$> parseTimeM True tl fmt (unpack s)


writeLines :: [Text] -> IO ()
writeLines = mapM_ TIO.putStrLn
