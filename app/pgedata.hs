{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Main where

import System.Environment (getArgs)
import Data.Csv (HasHeader(..), decode)
import Data.Time
import Data.Text (Text, pack, isSuffixOf, isInfixOf)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import PGE

-- 0              1          2     3     4    5   6
-- Electric usage,2018-08-15,00:00,00:14,0.08,kWh,

-- 0                 1          2    3      4
-- Natural gas usage,2016-01-10,3.13,therms,$4.09,

process :: (Text -> Maybe Text) -> Text -> [V.Vector Text] -> [Text]
process tparse site =
  concatMap rewrite . filter matches

  where matches :: V.Vector Text -> Bool
        matches r =
          "usage" `isSuffixOf` (V.head r)

        rewrite :: V.Vector Text -> [Text]
        rewrite r =
          case tparse (ts (etype r) r) of
            Nothing -> []
            Just t -> ["energy,site=" <> site <> ",energy_type=" <> etype r <> ",value_type=usage " <>
                        "value=" <> val (etype r) r <> " " <> t]

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

main :: IO ()
main = do
  [env] <- getArgs
  tz <- getCurrentTimeZone
  csvData <- BL.getContents
  case decode NoHeader csvData :: Either String (V.Vector (V.Vector Text)) of
    Left err -> fail (show err)
    Right v -> writeLines $ process (tsToUnix tz "%Y-%-m-%-d %H:%M") (pack env) (V.toList v)
