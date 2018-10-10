{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Main where

import System.Environment (getArgs)
import Data.Csv (HasHeader(..), decode)
import Data.Time
import Data.Text (Text, pack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import PGE

-- "Timestamp","Type","Device GUID","Energy In Interval (kWh)","Lifetime Cumulative Energy (kWh)","Current","Voltage"
-- 0                   1            2                                    3    4     5    6
-- 2018-10-08T07:30:00,"Production",22ab5fe1-00e2-4719-89f0-2e3941b20753,0.00,17.18,0.92,244.60


-- print "energy,site=%s,energy_type=gas,value_type=usage value=%s %s" % (site, row[2], timestamp)
-- print "energy,site=%s,energy_type=electric,value_type=usage value=%s %s" % (site, row[4], timestamp)

process :: (Text -> Maybe Text) -> Text -> [V.Vector Text] -> [Text]
process tparse site rows =
  (concatMap rewrite . filter matches) rows

  where matches :: V.Vector Text -> Bool
        matches r = r V.! 1 == "Production"

        rewrite :: V.Vector Text -> [Text]
        rewrite r =
          case tparse (V.head r) of
            Nothing -> []
            Just t -> ["energy,site=" <> site <> ",energy_type=solar,value_type=production " <>
                        "value=" <> r V.! 3 <> " " <> t,
                        "energy,site=" <> site <> ",energy_type=solar,value_type=current " <>
                        "value=" <> r V.! 5 <> " " <> t,
                        "energy,site=" <> site <> ",energy_type=solar,value_type=voltage " <>
                        "value=" <> r V.! 6 <> " " <> t
                      ]

main :: IO ()
main = do
  [env] <- getArgs
  tz <- getCurrentTimeZone
  csvData <- BL.getContents
  case decode NoHeader csvData :: Either String (V.Vector (V.Vector Text)) of
    Left err -> fail (show err)
    Right v -> writeLines $ process (tsToUnix tz "%Y-%-m-%-dT%H:%M:00") (pack env) (V.toList v)
