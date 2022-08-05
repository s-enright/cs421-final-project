import Core
import Parse
import Tests

import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure), exitSuccess)
import System.Timeout (timeout)
import Control.Exception (try, evaluate, SomeException)

-- Main outline of entry point to tests taken from MP3
main =
   do putStrLn "\nRunning Tests"
      putStrLn "============="
      results <- runTests
      mapM_ (putStrLn . showTR) results
      putStrLn ""
      let (scoreNum, testNum, scorePct) = scoreResult results
      putStrLn $ show scoreNum ++ " of " ++ show testNum ++ " tests passed "  
                 ++ "(" ++ show scorePct ++ "% success rate)\n"
      if scorePct >= 100
         then do putStrLn "All tests passed."
                 exitSuccess
         else do putStrLn "Some tests failed."
                 exitWith $ ExitFailure 1

-- Status of an individual test run
data TestStatus = Pass
                | Fail [Bool]
                | Exception
                | Timeout
                deriving Show

-- Result of a test and its name
type TestResult = (TestStatus, String)

-- Display the score and name of a test
showTR :: TestResult -> String
showTR (score, name) = show score ++ ": " ++ name

-- Display percent successful
scoreResult :: [(TestStatus, b)] -> (Int, Int, Int)
scoreResult tests = let passed = length [t | t@(Pass, _) <- tests]
                in  (passed, length tests, (passed * 100) `div` length tests)

-- Execute all tests and return a list of TestResults instances,
-- proving the result of the test and its name
runTests :: IO [TestResult]
runTests = let handleTest (test, name) = do score <- runTest test
                                            return (score, name)
           in  mapM handleTest allTests

-- Execute an individual test, returning its result status
runTest :: [Bool] -> IO TestStatus
runTest tests =
   let test  = case and tests of
               True  -> Pass
               False -> Fail tests
       onExn = const Exception :: SomeException -> TestStatus
       tryT  = try (evaluate test) >>= return . either onExn id
   in  timeout 1000000 tryT >>= return . maybe Timeout id