module WithTiming.Shell (execShell, getFullPath) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text              as T
import           System.Directory       (getHomeDirectory)
import           System.FilePath        (joinPath, splitDirectories)
import qualified Turtle                 as SH

-- | Returns 'True' on success, 'False' on failure.
execShell :: MonadIO io => T.Text -> io Bool
execShell shell = do
  exitCode <- SH.shell shell SH.empty
  case exitCode of
    SH.ExitSuccess   -> return True
    SH.ExitFailure _ -> return False

-- | Replaces a leading "~" segment with the user's actual home directory.
getFullPath :: MonadIO io => FilePath -> io FilePath
getFullPath path = do
  home <- liftIO $ getHomeDirectory
  case splitDirectories path of
    ("~":rest) -> return $ joinPath (home:rest)
    _ -> return path
