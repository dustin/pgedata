{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative (liftA2)
import           Data.List.Extra     (chunksOf)
import           Data.Time.Clock     (getCurrentTime, utctDay)
import           System.Environment  (getArgs)

import           DB                  (EnergyType (..), QueryParams, Site (..),
                                      lastTimestamp, myQueryParams, toLocal)

mkSolarURLs :: (Show a, Enum a) => String -> a -> a -> [String]
mkSolarURLs uid f t =
  let days = map (liftA2 (,) head last) . chunksOf 3 $ [pred f .. t] in
    map (\(s,e) -> mconcat [
            "https://mysolarcity.com/solarcity-api/powerguide/v1.0/installations/", uid,
            "/summary?StartTime=", show s, "T00:00:00&EndTime=",
            show e, "T23:59:59&Period=QuarterHour"]) days

showStuff :: QueryParams -> IO ()
showStuff p = do
  [uid] <- getArgs
  lastd <- pred . utctDay <$> lastTimestamp p Solar SJ
  today <- utctDay <$> getCurrentTime
  mapM_ putStrLn $ mkSolarURLs uid lastd today

  putStrLn ""

  sjts <- ts Electric SJ
  putStrLn $ "Last from San Jose: " <> sjts
  ots <- ts Electric Oro
  putStrLn $ "Last from Oroville: " <> ots

  where
    ts :: EnergyType -> Site -> IO String
    ts t s = lastTimestamp p t s >>= toLocal >>= \s' -> pure $ show s'


main :: IO ()
main = showStuff =<< myQueryParams
