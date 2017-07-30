module WithTiming.Prediction
  ( getCurrentTime
  , getLocalTZ
  , getReadableEstimate
  , showDiff
  , readableEstimate
  , getSecondsSince
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.List              (intercalate)
import           Data.Time              (NominalDiffTime, UTCTime)
import qualified Data.Time              as T
import           Data.Time.Format       (defaultTimeLocale, formatTime)

-- |A wrapper around 'Data.Time.getCurrentTime' that uses MonadIO.
getCurrentTime :: MonadIO io => io UTCTime
getCurrentTime = liftIO T.getCurrentTime

-- |Returns the current local time zone.
getLocalTZ :: MonadIO io => io T.TimeZone
getLocalTZ = do
  now <- getCurrentTime
  liftIO $ T.getTimeZone now

toSeconds :: NominalDiffTime -> Integer
toSeconds diff =
  let secs = filter (/= 's') (show diff) in
    read secs

pad :: Integer -> String
pad n | n < 10 = '0':show n
      | otherwise = show n

type ExpandedDiffTime = (Integer, Integer, Integer)

expandDiffTime :: NominalDiffTime -> ExpandedDiffTime
expandDiffTime diffTime =
  let total = toSeconds diffTime
      seconds = total `rem` 60
      minutes = (total `div` 60) `mod` 60
      hours   = total `div` (60 * 60)
  in
    (hours, minutes, seconds)

sepTimes :: [String] -> String
sepTimes = intercalate ":"

showDiff' :: ExpandedDiffTime -> String
showDiff' (0,0,1)                 = "1 second"
showDiff' (0,0,seconds)           = show seconds ++ " seconds"
showDiff' (0,minutes,seconds)     = sepTimes [show minutes, pad seconds]
showDiff' (hours,minutes,seconds) = sepTimes [show hours, pad minutes, pad seconds]

showDiff :: NominalDiffTime -> String
showDiff = showDiff' . expandDiffTime

-- |Given an expected number of seconds, the local time zone, and the current
-- time, return a string representing a (user-friendly) prediction for when a
-- comparable process will end if it starts now.
--
-- note: requiring an explicit TZ here is necessary to avoid IO
readableEstimate :: Integer -> T.TimeZone -> UTCTime -> String
readableEstimate previous tz start = do
  let diff = fromInteger previous
  mconcat ["The previous run finished in ", (showDiff diff), ".\n",
           "That suggests that this run will finish around ", estimateTime diff start, "."]
  where
   toTwelveHour :: T.FormatTime t => t -> String
   toTwelveHour = formatTime defaultTimeLocale "%r"
   estimateTime ::  NominalDiffTime -> UTCTime -> String
   estimateTime diff start = toTwelveHour $ T.utcToLocalTime tz $ diff `T.addUTCTime` start

-- |A shortcut for calling 'readableEstimate' with IO.
getReadableEstimate :: MonadIO io => Integer -> io String
getReadableEstimate previous = do
  start <- getCurrentTime
  tz <- getLocalTZ
  return $ readableEstimate previous tz start

-- |Returns the (rounded) whole number of seconds since 'start'.
getSecondsSince :: MonadIO io => UTCTime -> io Integer
getSecondsSince start = do
  now <- getCurrentTime
  let diff = T.diffUTCTime now start
  return (round diff)
