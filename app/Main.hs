{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Main where

import System.Environment (getArgs)
import Data.Csv (HasHeader(..), decode)
import Data.Time
import Data.Text (Text, unpack, pack, isSuffixOf, isInfixOf)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import Data.List (intercalate)

import PGE

-- 0              1          2     3     4    5   6
-- Electric usage,2018-08-15,00:00,00:14,0.08,kWh,

-- 0                 1          2    3      4
-- Natural gas usage,2016-01-10,3.13,therms,$4.09,

-- print "energy,site=%s,energy_type=gas,value_type=usage value=%s %s" % (site, row[2], timestamp)
-- print "energy,site=%s,energy_type=electric,value_type=usage value=%s %s" % (site, row[4], timestamp)

process :: TimeZone -> Text -> [V.Vector Text] -> [Text]
process tz site rows =
  (filter (/= "") . map rewrite . filter matches) rows

  where matches :: V.Vector Text -> Bool
        matches r =
          "usage" `isSuffixOf` (V.head r)

        rewrite :: V.Vector Text -> Text
        rewrite r =
          case tsToUnix tz (ts (etype r) r) of
            Nothing -> ""
            Just t -> "energy,site=" <> site <> ",energy_type=" <> etype r <> ",value_type=usage " <>
                       "value=" <> val (etype r) r <> " " <> t

        etype :: V.Vector Text -> Text
        etype r
          | "gas" `isInfixOf` V.head r = "gas"
          | otherwise = "electric"

        val "gas" r = r V.! 2
        val "electric" r = r V.! 4
        val _ _ = undefined

        ts "gas" r = r V.! 1 <> " 00:00"
        ts "electric" r = (r V.! 1) <> " " <> r V.! 2
        ts _ _ = undefined

encode :: [Text] -> BL.ByteString
encode rows = BL.fromStrict $ BC.pack $ intercalate "\n" $ (map unpack) rows

main :: IO ()
main = do
  [env] <- getArgs
  tz <- getCurrentTimeZone
  csvData <- BL.getContents
  case decode NoHeader csvData :: Either String (V.Vector (V.Vector Text)) of
    Left err -> fail (show err)
    Right v -> (BL.putStr . encode) $ process tz (pack env) (V.toList v)
