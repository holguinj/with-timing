module Main where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe             (fromMaybe)
import           Data.Semigroup         ((<>))
import qualified Data.Text              as T
import           Options.Applicative    hiding (command)
import           WithTiming.Program     (basic)
import           WithTiming.RunProgram  (runShellJSON)
import           WithTiming.Shell       (execShell, getFullPath)
import           WithTiming.Storage     (getPreviousResult, updateResultFile)

data BaseArgs = BaseArgs
  { _file    :: String
  , _key     :: Maybe String
  , _command :: String
  } deriving (Show)

data NormalArgs = NormalArgs
  { file    :: FilePath
  , key     :: String
  , command :: T.Text
  }

normalize :: MonadIO io => BaseArgs -> io NormalArgs
normalize args = do
  fullPath <- getFullPath (_file args)
  return $ NormalArgs
    { file = fullPath
    , key = fromMaybe (_command args) (_key args)
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

argParse :: Parser BaseArgs
argParse = BaseArgs
        <$> strOption fileOption
        <*> optional (strOption keyOption)
        <*> strArgument commandArgument

main :: IO ()
main = runArgs =<< execParser opts
  where
    opts = info (argParse <**> helper)
      ( fullDesc
     <> progDesc "Run a command with timing and prediction."
     <> header "with-timing - a utility for timing and prediction of shell commands.")

runArgs :: BaseArgs -> IO ()
runArgs baseArgs = do
  args <- normalize baseArgs
  let program = basic (key args) (command args)
  runShellJSON (file args) program
