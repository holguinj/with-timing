module WithTiming.Shell (execShell, getFullPath) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text              as T
import           System.Directory       (getHomeDirectory)
import           System.Exit            (ExitCode)
import           System.FilePath        (joinPath, splitDirectories)
import qualified Turtle                 as SH

-- | Small wrapper around Turtle.shell
execShell :: MonadIO io => T.Text -> io ExitCode
execShell shell = SH.shell shell SH.empty

-- | Replaces a leading "~" segment with the user's actual home directory.
getFullPath :: MonadIO io => FilePath -> io FilePath
getFullPath path = do
  home <- liftIO $ getHomeDirectory
  case splitDirectories path of
    ("~":rest) -> return $ joinPath (home:rest)
    _          -> return path
