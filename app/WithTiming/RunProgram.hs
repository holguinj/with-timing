module WithTiming.RunProgram (runShellJSON) where

import           Control.Monad.Free
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Time              (UTCTime)
import           System.Exit            (ExitCode)
import           WithTiming.Prediction  (getCurrentTime, getLocalTZ,
                                         getReadableEstimate, getSecondsSince,
                                         readableEstimate)
import           WithTiming.Program     (Command (..), Program)
import           WithTiming.Shell       (execShell)
import           WithTiming.Storage     as S

-- | An interpreter for the Program type that:
--   1. Uses Data.Time.UTCTime to track timing.
--   2. Uses JSON to differentiate keys.
--   3. Actually executes shell actions.
runShellJSON :: MonadIO io => FilePath -> Program UTCTime ExitCode -> io ExitCode
runShellJSON file prog = case prog of
  Free (ReadPrevious key g) -> do
    prev <- S.getPreviousResult file key
    runShellJSON file $ g prev
  Free (Predict mdur next) -> do
    case mdur of
      Nothing -> liftIO $ putStrLn "Unable to find a previous run."
      Just dur -> do
        estimate <- getReadableEstimate dur
        liftIO $ putStrLn estimate
    runShellJSON file next
  Free (BeginTimer g) -> do
    now <- getCurrentTime
    runShellJSON file $ g now
  Free (Execute shell g) -> do
    exitCode <- execShell shell
    runShellJSON file $ g exitCode
  Free (SecondsSince time g) -> do
    diff <- getSecondsSince time
    runShellJSON file $ g diff
  Free (Inform msg next) -> do
    liftIO $ putStrLn msg
    runShellJSON file next
  Free (WriteResult key dur next) -> do
    S.updateResultFile file key dur
    runShellJSON file next
  Pure exitCode -> return exitCode
