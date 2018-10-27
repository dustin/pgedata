{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Main where

import System.Environment (getArgs)
import Data.Text (Text, pack)
import qualified Data.Vector as V

import PGE

-- "Timestamp","Type","Device GUID","Energy In Interval (kWh)","Lifetime Cumulative Energy (kWh)","Current","Voltage"
-- 0                   1            2                                    3    4     5    6
-- 2018-10-08T07:30:00,"Production",22ab5fe1-00e2-4719-89f0-2e3941b20753,0.00,17.18,0.92,244.60

main :: IO ()
main = do
  [site] <- getArgs
  process "%Y-%-m-%-dT%H:%M:00" matches (rewrite (pack site))

  where matches :: FilterFun
        matches r = r V.! 1 == "Production"

        rewrite :: Text -> RewriteFun
        rewrite site tparse r =
          case tparse (V.head r) of
            Nothing -> []
            Just t -> ["energy,site=" <> site <> ",energy_type=solar,value_type=production " <>
                        "value=" <> r V.! 3 <> " " <> t,
                        "energy,site=" <> site <> ",energy_type=solar,value_type=current " <>
                        "value=" <> r V.! 5 <> " " <> t,
                        "energy,site=" <> site <> ",energy_type=solar,value_type=voltage " <>
                        "value=" <> r V.! 6 <> " " <> t
                      ]
