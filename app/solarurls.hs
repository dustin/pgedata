module Main where

import Control.Applicative (liftA2)
import Control.Monad (forM_)
import Data.List.Extra (chunksOf)
import Data.Time.Clock (getCurrentTime, utctDay)
import System.Environment (getArgs)

down :: Enum a => a -> [a]
down d = enumFromThen d (pred d)

main :: IO ()
main = do
  [uid] <- getArgs
  days <- map (liftA2 (,) last head) . chunksOf 3 . take 14 . down . pred . utctDay <$> getCurrentTime
  forM_ days $ \(s,e) ->
    putStrLn $ mconcat [
      "https://mysolarcity.com/solarcity-api/powerguide/v1.0/installations/", uid,
      "/summary?StartTime=", show s, "T00:00:00&EndTime=", show e, "T23:59:59&Period=QuarterHour"]
