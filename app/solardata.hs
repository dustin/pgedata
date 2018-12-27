{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import qualified Data.Map.Strict    as Map
import           Data.Text          (Text, unpack)
import           Data.Time          (UTCTime)
import qualified Data.Vector        as V
import           Database.InfluxDB
import           System.Environment (getArgs)
import           System.IO          (IOMode (..), withFile)

import           PGE

-- withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
fileToLines :: String  -> IO [Line UTCTime]
fileToLines fn = withFile fn ReadMode $ \h -> do
  (Right csvd) <- parseCSV h
  tparse <- mkTimeParser "%Y-%-m-%-dT%H:%M:00"
  pure $ concatMap (row tparse) csvd

    where
      row :: (Text -> Maybe UTCTime) -> V.Vector Text -> [Line UTCTime]
      row tparse r = case tparse (V.head r) of
                       Nothing -> []
                       t       -> [mkLine 3 t "production",
                                   mkLine 5 t "current",
                                   mkLine 6 t "voltage"]

        where
          mkLine p t vt = Line "energy" (Map.fromList [("site", "sj"),
                                                       ("energy_type", "solar"),
                                                       ("value_type", vt)])
                           (Map.singleton "value" (FieldFloat . read.unpack $ r V.! p)) t

main :: IO ()
main = do
  fns <- getArgs
  rose <- mconcat <$> traverse fileToLines fns
  let wp = writeParams "pge" & precision .~ Minute

  writeBatch wp rose
