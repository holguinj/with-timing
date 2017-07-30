module WithTiming.Programs (basic, allowAnyExitCode) where

import qualified Data.Text             as T
import           System.Exit           (ExitCode (..))
import           WithTiming.Prediction (showDiff)
import           WithTiming.Program

successful :: ExitCode -> Bool
successful ExitSuccess = True
successful _           = False

showSeconds :: Integer -> String
showSeconds = showDiff . fromInteger

-- | A typical example of a program constructed in the 'Program' type.
basic :: Key -> T.Text -> Program time ExitCode
basic key command = do
  prev <- readPrevious key
  predict prev
  start <- beginTimer
  exitCode <- execute command
  if (successful exitCode) then do
    time <- secondsSince start
    inform $ "Command executed successfully in " ++ showSeconds time ++ "."
    writeResult key time
  else
    inform "Command failed! Not recording results."
  return exitCode

-- | A program that informs the user of abnormal exit codes, but records the results nonetheless.
allowAnyExitCode :: Key -> T.Text -> Program time ExitCode
allowAnyExitCode key command = do
  prev <- readPrevious key
  predict prev
  start <- beginTimer
  exitCode <- execute command
  time <- secondsSince start
  case exitCode of
    ExitSuccess -> inform $ "Command executed successfully in " ++ showSeconds time ++ "."
    ExitFailure num -> inform $ "Command exited abnormally (" ++ show num ++ ") in " ++ showSeconds time ++ "."
  writeResult key time
  return exitCode
