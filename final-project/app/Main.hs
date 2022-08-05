module Main where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Parse ( pInputString, parse )
import Core ( Result(Parsed), Types(IntExp, Command, BoolExp) )


-- Prompt user input and flush stream
prompt :: String -> IO ()
prompt str = putStr str >> hFlush stdout

-- Display a string and flush stream
printLn :: String -> IO ()
printLn str = putStr str >> hFlush stdout

-- REPL for packrat parser
repl :: IO ()
repl = do input <- prompt "Packrat> " >> getLine
          case input of
            "quit" -> return ()
            "exit" -> return ()
            _      -> case pInputString (parse input) of
                           (Parsed Command _) -> repl
                           (Parsed (BoolExp b) _) -> do print b
                                                        repl
                           (Parsed (IntExp i) _) -> do print i
                                                       repl
                           _ -> error "Parse error"

-- Main entry into program
main :: IO ()
main = do putStrLn "Staring Packrat REPL...\n"
          repl
          putStrLn "\n...Leaving Packrat REPL"