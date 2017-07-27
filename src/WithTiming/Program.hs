{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module WithTiming.Program
  ( Command(..)
  , InterpretedCommand(..)
  , interpretPure
  , Program
  , basic
  , readPrevious
  , predict
  , beginTimer
  , execute
  , secondsSince
  , inform
  , writeResult
  ) where

import           Control.Monad.Free (Free (..), liftF)
import qualified Data.Text          as T
import           System.Exit        (ExitCode (..))

type Key = String

successful :: ExitCode -> Bool
successful ExitSuccess = True
successful _ = False

-- | A type representing steps in a typical program execution. Used in the
-- Program monad via functions with names that correspond to the type
-- constructors here.
--
-- The 'time' parameter refers to the type representing timing information.
--
-- The 'next' parameter is recursive and should be of no immediate concern
-- except for the Functor implementation.
data Command time next =
    ReadPrevious Key (Maybe Integer -> next)
  -- ^ Read a (possibly missing) Integer value representing the duration the last time 'key' ran.
  | Predict (Maybe Integer) next
  -- ^ Output a prediction for the current run, which may not have a precedent.
  | BeginTimer (time -> next)
  -- ^ Return an object referring to the start time of the command.
  | Execute T.Text (ExitCode -> next)
  -- ^ Perform the shell action, returning a result in the required type.
  | SecondsSince time (Integer -> next)
  -- ^ Determine the number of seconds that have passed since the 'time' object was created.
  | Inform String next
  -- ^ Display the given string to the user.
  | WriteResult Key Integer next
  -- ^ Record the results of the current run.
  deriving (Functor)

-- | A (Free) monad for constructing a sequence of Commands.
-- The type is parameterized here by 'shellResult', but instances will also be
-- parameterized over the Pure return type from Free.
type Program time = Free (Command time)

-- | Read a (possibly missing) Integer value representing the duration the last time 'key' ran.
readPrevious :: Key -> Program time (Maybe Integer)
readPrevious key = liftF (ReadPrevious key id)

-- | Output a prediction for the current run, which may not have a precedent.
predict :: Maybe Integer -> Program time ()
predict mdur = liftF (Predict mdur ())

-- | Return an object referring to the start time of the command.
beginTimer :: Program time time
beginTimer = liftF (BeginTimer id)

-- | Perform the shell action, returning a result in the required type.
execute :: T.Text -> Program time ExitCode
execute shell = liftF (Execute shell id)

-- | Determine the number of seconds that have passed since the 'time' object was created.
secondsSince :: time -> Program time Integer
secondsSince time = liftF (SecondsSince time id)

-- | Display the given string to the user.
inform :: String -> Program time ()
inform msg = liftF (Inform msg ())

-- | Record the results of the current run.
writeResult :: Key -> Integer -> Program time ()
writeResult key duration = liftF (WriteResult key duration ())

showSeconds :: Integer -> String
showSeconds n =
  let secs = filter (not . (==) '"') (show n) in
  secs ++ " seconds"

-- | A typical example of a program constructed in the 'Program' type.
basic :: Key -> T.Text -> Program time ExitCode
basic key command = do
  prev <- readPrevious key
  predict prev
  start <- beginTimer
  exitCode <- execute command
  if (successful exitCode) then do
    time <- secondsSince start
    inform $ "Command executed successfully in " ++ (showSeconds time) ++ "."
    writeResult key time
  else
    inform "Command failed! Not recording results."
  return exitCode

-- | A minimal target for interpreting a Program, useful for tests and
-- debugging. Represents concrete actions that are the result of
-- (hypothetically) running the Program.
data InterpretedCommand =
    ReadingPrevious Key
  | Predicting (Maybe Integer)
  | BeginningTimer
  | Executing T.Text
  | CountingSeconds Integer
  | Informing String
  | WritingResult Key Integer
  | Returning ExitCode
  deriving (Show, Eq)

-- | An example interpreter that reduces the commands to ['InterpretedCommand'].
-- intended to be used in testing and debugging. Uses mocked values instead of
-- side-effects.
interpretPure :: (Maybe Integer)      -- ^ The result of looking up a key
              -> Integer              -- ^ The number of seconds returned by 'secondsSince'
              -> ExitCode             -- ^ The result of the shell command
              -> Program () ExitCode  -- ^ A program that uses () for its time type and returns an 'ExitCode'
              -> [InterpretedCommand] -- ^ A list of concrete actions resulting from the program.
interpretPure keyLookup seconds exitCode prog = case prog of
  Free (ReadPrevious key g) -> ReadingPrevious key : recur (g keyLookup)
  Free (Predict mdur next) -> Predicting mdur : recur next
  Free (BeginTimer g) -> BeginningTimer : recur (g ())
  Free (Execute shell g) -> Executing shell : recur (g exitCode)
  Free (SecondsSince time g) -> CountingSeconds seconds : recur (g seconds)
  Free (Inform msg next) -> Informing msg : recur next
  Free (WriteResult key dur next) -> WritingResult key dur : recur next
  Pure r -> [Returning r]
  where
    recur = interpretPure keyLookup seconds exitCode
