module Main where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Maybe             (fromMaybe)
import           Data.Semigroup         ((<>))
import qualified Data.Text              as T
import           Options.Applicative    hiding (command)
import           System.Exit            (ExitCode (..), exitWith)
import           WithTiming.Program     (Program)
import qualified WithTiming.Programs    as Programs
import           WithTiming.RunProgram  (runShellJSON)
import           WithTiming.Shell       (getFullPath)

data BaseArgs = BaseArgs
  { _file         :: String
  , _key          :: Maybe String
  , _allowAnyExit :: Bool
  , _command      :: String
  }

data NormalArgs = NormalArgs
  { file         :: FilePath
  , key          :: String
  , allowAnyExit :: Bool
  , command      :: T.Text
  } deriving (Show)

normalize :: MonadIO io => BaseArgs -> io NormalArgs
normalize args = do
  fullPath <- getFullPath (_file args)
  return NormalArgs
    { file = fullPath
    , key = fromMaybe (_command args) (_key args)
    , allowAnyExit = _allowAnyExit args
    , command = T.pack (_command args)
    }

commandArgument :: Mod ArgumentFields a
commandArgument = metavar "COMMAND"
               <> help "The command to run with timing"

fileOption :: Mod OptionFields String
fileOption = long "file"
          <> short 'f'
          <> value "~/.config/with-timing.json"
          <> showDefault
          <> metavar "FILEPATH"
          <> help "The file where timing data is stored"

keyOption :: Mod OptionFields a
keyOption = long "key"
         <> short 'k'
         <> metavar "KEY"
         <> help "The key under which timing for this command is stored (defaults to the full command)"

allowAnyExitFlag :: Mod FlagFields Bool
allowAnyExitFlag = long "allow-any-exitcode"
                <> short 'a'
                <> showDefault
                <> help "Record timing even if the command exits with a nonzero exitcode"

argParse :: Parser BaseArgs
argParse = BaseArgs
        <$> strOption fileOption
        <*> optional (strOption keyOption)
        <*> switch allowAnyExitFlag
        <*> strArgument commandArgument

buildProgram :: NormalArgs -> Program a ExitCode
buildProgram args =
  let build =
        if allowAnyExit args
          then Programs.basic
          else Programs.allowAnyExitCode
  in build (key args) (command args)

runArgs :: BaseArgs -> IO ()
runArgs baseArgs = do
  args <- normalize baseArgs
  let program = buildProgram args
  exitCode <- runShellJSON (file args) program
  exitWith exitCode

main :: IO ()
main = runArgs =<< execParser opts
  where
    opts = info (argParse <**> helper)
      ( fullDesc
     <> progDesc "Run a command with timing and prediction."
     <> header "with-timing - a utility for timing and prediction of shell commands.")
