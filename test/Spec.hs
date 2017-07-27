import           Test.Hspec

import qualified Data.Text           as T
import           System.Exit         (ExitCode (..))
import           WithTiming.Program
import           WithTiming.Programs

main :: IO ()
main = hspec $ do
  let key = "TestKey"
  let previous = (Just 1)
  let duration = 2
  let goodExit = ExitSuccess
  let badExit = ExitFailure 666
  let command = T.pack "man splain"
  describe "basic program" $ do
    describe "on a successful run" $ do
      let successful = interpretPure previous duration goodExit (basic key command)
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
      let failing = interpretPure previous duration badExit (basic key command)
      it "does not record the result" $ do
        not (writesSomeResult failing)
      it "exits as expected" $ do
        failing `exitsWith` badExit
      describe "unless failure should be tolerated, in which case" $ do
        let failingButOk = interpretPure previous duration badExit (allowAnyExitCode key command)
        it "should record a result anyway" $ do
          writesSomeResult failingButOk
        it "should report the exit code" $ do
          failingButOk `informs` "666"
        it "should exit with the command's exit code" $ do
          failingButOk `exitsWith` badExit
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

writesSomeResult :: [InterpretedCommand] -> Bool
writesSomeResult [] = False
writesSomeResult (WritingResult _ _:cs) = True
writesSomeResult (_:cs) = writesSomeResult cs

informs :: [InterpretedCommand] -> String -> Bool
informs prog substring =
  any isMatch (allStrings prog)
  where
    allStrings [] = []
    allStrings (Informing str:cs) = str:allStrings cs
    allStrings (_:cs) = allStrings cs
    isMatch str = case T.breakOnAll (T.pack substring) (T.pack str) of
                    [] -> False
                    _  -> True
