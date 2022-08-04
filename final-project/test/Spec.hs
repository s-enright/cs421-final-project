import Core
import Parse
import Tests

import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.Timeout (timeout)
import Control.Exception (try, evaluate, SomeException)

-- Main outline taken from MP3
main =
   do putStrLn ""
      putStrLn "Running Tests"
      putStrLn "============="
      results <- runTests
      mapM_ (putStrLn . showTR) results
      putStrLn ""
      let score = scorePct results
      putStrLn $ "Score: " ++ show score ++ " / 100"
      if score >= 100
         then do putStrLn "All tests passed."
                 exitWith ExitSuccess
         else do putStrLn "Some tests failed."
                 exitWith $ ExitFailure 1

-- Status of an individual test run
data TestStatus = Pass
                | Fail [Bool]
                | Exception
                | Timeout
                deriving Show

type TestResult = (TestStatus, String)

showTR :: TestResult -> String
showTR (score, name) = show score ++ ": " ++ name

-- Display percent successful
scorePct :: [TestResult] -> Int
scorePct tests = let passed = length [t | t@(Pass, _) <- tests]
                in  (passed * 100) `div` (length tests)

runTests :: IO [TestResult]
runTests = let handleTest (test, name) = do score <- runTest test
                                            return (score, name)
           in  mapM handleTest allTests

runTest :: [Bool] -> IO TestStatus
runTest tests = 
   let test  = case and tests of
               True  -> Pass
               False -> Fail tests
       onExn = const Exception :: SomeException -> TestStatus
       tryT  = try (evaluate test) >>= return . either onExn id
   in  timeout 1000000 tryT >>= return . maybe Timeout id