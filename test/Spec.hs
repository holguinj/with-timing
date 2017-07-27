import           Test.Hspec

import qualified Data.Text          as T
import           System.Exit        (ExitCode (..))
import           WithTiming.Program

main :: IO ()
main = hspec $ do
  describe "basic program" $ do
    let key = "TestKey"
    let previous = (Just 1)
    let duration = 2
    let goodExit = ExitSuccess
    let badExit = ExitFailure 666
    let command = T.pack "man splain"
    let successful = interpretPure previous duration goodExit (basic key command)
    let failing = interpretPure previous duration badExit (basic key command)
    describe "on a successful run" $ do
      it "reads the previous value" $ do
        successful `hasCommand` (ReadingPrevious key)
      it "makes a prediction based on that value" $ do
        successful `hasCommand` (Predicting previous)
      it "begins the timer" $ do
        successful `hasCommand` BeginningTimer
      it "executes the command" $ do
        successful `hasCommand` (Executing command)
      it "calculates the duration" $ do
        successful `hasCommand` (CountingSeconds duration)
      it "writes the result" $ do
        successful `hasCommand` (WritingResult key duration)
      it "exits as expected" $ do
        successful `exitsWith` goodExit
    describe "on a failing run" $ do
      it "does not record the result" $ do
        not (writesAnyResult failing)
      it "exits as expected" $ do
        failing `exitsWith` badExit
    describe "when there's no previous value" $ do
      let noPrevious = interpretPure Nothing duration goodExit (basic key command)
      it "still calls the prediction function" $ do
        noPrevious `hasCommand` (Predicting Nothing)
      it "executes the command" $ do
        noPrevious `hasCommand` (Executing command)



-- Helpers

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

hasCommand :: [InterpretedCommand] -> InterpretedCommand -> Bool
hasCommand prog cmd =
  filter (== cmd) prog
  |> length
  |> (== 1)

exitsWith :: [InterpretedCommand] -> ExitCode -> Bool
exitsWith prog exitCode = prog `hasCommand` (Returning exitCode)

writesAnyResult :: [InterpretedCommand] -> Bool
writesAnyResult [] = False
writesAnyResult (WritingResult _ _:cs) = True
writesAnyResult (_:cs) = writesAnyResult cs
