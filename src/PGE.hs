{-# LANGUAGE OverloadedStrings #-}

module PGE where

import Control.Monad (mapM_)
import Data.Csv (HasHeader(..), decode)
import Data.List (intercalate)
import Data.Text (Text, unpack, pack)
import Data.Time
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V

tsToUnix :: TimeZone -> String -> Text -> Maybe Text
tsToUnix tz fmt s =
  let tl = defaultTimeLocale in
    pack <$> formatTime tl "%s000000000" <$> localTimeToUTC tz <$> parseTimeM True tl fmt (unpack s)

type FilterFun = (V.Vector Text -> Bool)

type RewriteFun = (Text -> Maybe Text) -> V.Vector Text -> [Text]

process :: String -> FilterFun -> RewriteFun -> IO ()
process tfmt ff rf = do
  tz <- getCurrentTimeZone
  let tparse = tsToUnix tz tfmt
  csvData <- BL.getContents
  case decode NoHeader csvData :: Either String (V.Vector (V.Vector Text)) of
    Left err -> fail (show err)
    Right v -> mapM_ TIO.putStrLn $ (concatMap (rf tparse) . filter ff) (V.toList v)
