{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module WithTiming.Program
  ( Command(..)
  , Program
  , basic
  , readPrevious
  , predict
  , beginTimer
  , execute
  , secondsSince
  , inform
  , writeResult
  , stringify
  ) where

import           Control.Monad.Free (Free (..), liftF)
import qualified Data.Text as T

type Key = String
type Success = Bool

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
  | Execute T.Text (Success -> next)
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
execute :: T.Text -> Program time Success
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

example :: Program Integer ()
example = do
  let key = "example"
  prev <- readPrevious key
  predict prev
  start <- beginTimer
  success <- execute "echo 'hello'"
  if success then do
    time <- secondsSince start
    inform "command succeeded!"
    inform $ "that took " ++ (show time) ++ " seconds."
    writeResult key time
  else do
    inform "command failed :("
    inform "not recording results."

-- | An example interpreter that reduces the commands to [String].
-- Assumes 'Nothing' for a previous run, True for 'success', and 10s for timing.
stringify :: Show a => Program Integer a -> [String]
stringify prog = case prog of
  Free (ReadPrevious key g) -> ("Read " ++ key) : stringify (g Nothing)
  Free (Predict mdur g) -> ("Predicting based on " ++ (show mdur)) : stringify g
  Free (BeginTimer g) -> "Starting timer." : stringify (g 0)
  Free (Execute shell g) -> ("Executing: " ++ (show shell)) : stringify (g True)
  Free (SecondsSince time g) -> "Calculated difference of 10s." : stringify (g 10)
  Free (Inform msg next) -> ("Informing: " ++ msg) : stringify next
  Free (WriteResult key dur next) -> ("Writing " ++ (show dur) ++ " for key " ++ key) : stringify next
  Pure r -> ["Returns: " ++ (show r)]

showSeconds :: Integer -> String
showSeconds n =
  let secs = filter (not . (==) '"') (show n) in
  secs ++ " seconds"

basic :: Key -> T.Text -> Program time ()
basic key command = do
  prev <- readPrevious key
  predict prev
  start <- beginTimer
  success <- execute command
  if success then do
    time <- secondsSince start
    inform $ "Command executed successfully in " ++ (showSeconds time) ++ "."
    writeResult key time
  else do
    inform "Command failed! Not recording results."
