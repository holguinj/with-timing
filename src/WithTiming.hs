module WithTiming
   ( ResultWithOutput
   , SimpleResult
   , TimingResult
   , duration
   , exitCode
   , runShellWithTiming
   , stdErr
   , stdOut
    ) where

import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Text              as T

class TimingResult a where
  exitCode :: a -> Integer
  duration :: a -> Integer

data SimpleResult = SimpleResult { _exitCode :: Integer
                                 , _duration :: Integer
                                 }

instance TimingResult SimpleResult where
  exitCode = _exitCode
  duration = _duration

data ResultWithOutput = ResultWithOutput { _outputExitCode :: Integer
                                         , _outputDuration :: Integer
                                         , stdOut          :: T.Text
                                         , stdErr          :: T.Text
                                         }

instance TimingResult ResultWithOutput where
  exitCode = _outputExitCode
  duration = _outputDuration

runWithTiming :: MonadIO io => io () -> io SimpleResult
runWithTiming = undefined

runShellWithTiming :: MonadIO io => T.Text -> io SimpleResult
runShellWithTiming = undefined
