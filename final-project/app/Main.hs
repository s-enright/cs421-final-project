module Main where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Parse
import Core

-- Evaluate an expression and return the unpackaged result,
-- ignoring any unparsed remainder.
evalA s = case pAdditive (parse s) of
               Parsed v rem -> v
               _ -> error "Parse error"

evalK s = case pKeyword (parse s) of
            Parsed v rem -> v
            _ -> error "Parse error"
            
eval p s = case p (parse s) of
              Parsed v rem -> v
              _ -> error "Parse error"

--- The REPL
--- --------

prompt :: String -> IO ()
prompt str = putStr str >> hFlush stdout

printLn :: String -> IO ()
printLn str = putStr str >> hFlush stdout

repl :: IO ()
repl = do input <- prompt "> " >> getLine
          case input of
            "quit" -> return ()
            "exit" -> return ()
            _      -> case pValidInput (parse input) of
                           (Parsed Command _) -> repl
                           (Parsed (BoolExp b) _) -> do print b
                                                        repl
                           (Parsed (IntExp i) _) -> do print i
                                                       repl
                           _ -> error "Parse error"


main :: IO ()
main = do putStrLn "Staring Packrat REPL..."
          repl
          putStrLn "...Leaving Pack REPL"