{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Main where

import System.Environment (getArgs)
import Data.Text (Text, pack, isSuffixOf, isInfixOf)
import qualified Data.Vector as V

import PGE

-- 0              1          2     3     4    5   6
-- Electric usage,2018-08-15,00:00,00:14,0.08,kWh,

-- 0                 1          2    3      4
-- Natural gas usage,2016-01-10,3.13,therms,$4.09,

main :: IO ()
main = do
  [site] <- getArgs
  process "%Y-%-m-%-d %H:%M" matches (rewrite (pack site))

  where matches :: FilterFun
        matches r = "usage" `isSuffixOf` (V.head r)

        rewrite :: Text -> RewriteFun
        rewrite site tparse r =
          case tparse (ts (etype r) r) of
            Nothing -> []
            Just t -> ["enbergy,site=" <> site <> ",energy_type=" <> etype r <> ",value_type=usage " <>
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
