{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative (liftA2)
import           Control.Monad       (forM_)
import           Data.List.Extra     (chunksOf)
import           Data.Time.Clock     (getCurrentTime, utctDay)
import           System.Environment  (getArgs)

import           DB                  (EnergyType (..), Site (..), lastTimestamp,
                                      queryParams)

main :: IO ()
main = do
  [uid] <- getArgs
  lastd <- pred . utctDay <$> lastTimestamp (queryParams "pge") Solar SJ
  today <- utctDay <$> getCurrentTime
  let days = map (liftA2 (,) head last) . chunksOf 3 $ [lastd..today]
  forM_ days $ \(s,e) ->
    putStrLn $ mconcat [
      "https://mysolarcity.com/solarcity-api/powerguide/v1.0/installations/", uid,
      "/summary?StartTime=", show s, "T00:00:00&EndTime=", show e, "T23:59:59&Period=QuarterHour"]
