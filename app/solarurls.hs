module Main where

import Control.Applicative (liftA2)
import Control.Monad (forM_)
import Data.List.Extra (chunksOf)
import Data.Time.Clock (getCurrentTime, utctDay)
import System.Environment (getArgs)

main :: IO ()
main = do
  [start, uid] <- getArgs
  days <- map (liftA2 (,) head last) . chunksOf 3 . enumFromTo (read start) . utctDay <$> getCurrentTime
  forM_ days $ \(s,e) ->
    putStrLn $ mconcat [
      "https://mysolarcity.com/solarcity-api/powerguide/v1.0/installations/", uid,
      "/summary?StartTime=", show s, "T00:00:00&EndTime=", show e, "T23:59:59&Period=QuarterHour"]
