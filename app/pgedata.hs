{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Codec.Archive.Zip
import           Control.Lens
import           Control.Monad      (zipWithM)
import qualified Data.ByteString    as B
import           Data.Char          (toLower)
import           Data.Either        (either)
import qualified Data.Map           as Map
import           Data.Map.Strict    (Map)
import           Data.String        (fromString)
import           Data.Text          (Text, isInfixOf, isSuffixOf, pack, unpack)
import           Data.Time          (UTCTime)
import qualified Data.Vector        as V
import           Database.InfluxDB
import           System.Environment (getArgs)


import           DB
import           PGE

-- 0              1          2     3     4    5   6
-- Electric usage,2018-08-15,00:00,00:14,0.08,kWh,

-- 0                 1          2    3      4
-- Natural gas usage,2016-01-10,3.13,therms,$4.09,

acctMap :: Map Text Site
acctMap = Map.fromList [("8500", Oro), ("5074", SJ), ("8908", SJ)]

processIncr :: String -> B.ByteString -> IO [Line UTCTime]
processIncr fn d = do
  tparse <- mkTimeParser "%Y-%-m-%-d %H:%M"
  pure $ concatMap (row tparse) $ V.filter ff csvd

  where
    site = Map.foldrWithKey (\k v o -> if k `isInfixOf` pack fn then v else o) undefined acctMap
    csvd = either error id $ parseCSV' d

    ff :: V.Vector Text -> Bool
    ff r =  "usage" `isSuffixOf` V.head r

    row :: (Text -> Maybe UTCTime) -> V.Vector Text -> [Line UTCTime]
    row tparse r = case tparse (ts etype) of
                     Nothing -> []
                     t       -> [mkLine t]

      where

        mkLine t = Line "energy" (Map.fromList [("site", fromString . map toLower . show $ site),
                                                ("energy_type", fromString . unpack $ etype),
                                                ("value_type", "usage")])
                   (Map.singleton "value" (FieldFloat . read.unpack $ val etype)) t

        etype :: Text
        etype
          | "gas" `isInfixOf` V.head r = "gas"
          | otherwise = "electric"

        val "gas"      = r V.! 2
        val "electric" = r V.! 4
        val _          = undefined

        ts "gas"      = r V.! 1 <> " 00:00"
        ts "electric" = (r V.! 1) <> " " <> r V.! 2
        ts _          = undefined

doFile :: String -> IO [[Line UTCTime]]
doFile path = do
  fns <- withArchive path (Map.keys <$> getEntries)
  vs <- withArchive path (mapM getEntry fns)
  zipWithM processIncr (unEntrySelector <$> fns) vs

main :: IO ()
main = do
  fns <- getArgs
  lineses <- mapM doFile fns
  let rose = (mconcat . mconcat) lineses
  let wp = writeParams "pge" & precision .~ Minute
  writeBatch wp rose
