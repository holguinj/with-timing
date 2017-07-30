import           Test.Hspec

import qualified Data.Text             as T
import           Data.Time             (NominalDiffTime)
import           System.Exit           (ExitCode (..))
import           WithTiming.Prediction (showDiff)
import           WithTiming.Program
import           WithTiming.Programs

allTests :: [Spec]
allTests = [ programTest
           , showDiffTest
           ]

programTest :: Spec
programTest = do
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

showDiffTest :: Spec
showDiffTest = do
  let under60 = seconds 59
  let over60 = seconds 61
  let under1hour = seconds $ (60 * 60) - 1
  let over1hour = seconds $ (60 * 60) + 1
  let hour30 = seconds $ (60 * 60) + (60 * 30)
  describe "The showDiff function" $ do
    it "handles pluralization" $ do
      showDiff 0 `shouldBe` "0 seconds"
      showDiff 1 `shouldBe` "1 second"
    it "works for values under a minute" $ do
      showDiff under60 `shouldBe` "59 seconds"
    it "works for values over a minute" $ do
      showDiff over60 `shouldBe` "1:01"
    it "works for values under an hour" $ do
      showDiff under1hour `shouldBe` "59:59"
    it "works for values over one hour" $ do
      showDiff over1hour `shouldBe` "1:00:01"
    it "works for an hour and a half" $ do
      showDiff hour30 `shouldBe` "1:30:00"
  where
    seconds :: Integer -> NominalDiffTime
    seconds = fromInteger

main :: IO ()
main = do
  mapM_ hspec allTests

-- Helpers

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

hasCommand :: [InterpretedCommand] -> InterpretedCommand -> Bool
hasCommand prog cmd =
  filter (== cmd) prog
  |> length
  |> (== 1)

exitsWith :: [InterpretedCommand] -> ExitCode -> Bool
exitsWith prog exitCode = prog `hasCommand` Returning exitCode

writesSomeResult :: [InterpretedCommand] -> Bool
writesSomeResult []                     = False
writesSomeResult (WritingResult _ _:_)  = True
writesSomeResult (_:cs)                 = writesSomeResult cs

informs :: [InterpretedCommand] -> String -> Bool
informs prog substring =
  any isMatch (allStrings prog)
  where
    allStrings []                 = []
    allStrings (Informing str:cs) = str:allStrings cs
    allStrings (_:cs)             = allStrings cs
    isMatch str = case T.breakOnAll (T.pack substring) (T.pack str) of
                    [] -> False
                    _  -> True
