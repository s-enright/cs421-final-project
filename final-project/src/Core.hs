module Core where

import Control.Monad (liftM, ap)
import Data.Char ( isSpace, isDigit, digitToInt )
import Data.List (nub)
import Data.Bool (bool)


-- The Derivs type represents a column of the parsing results
-- matrix. Each field corresponds to a non-terminal, with a type
-- that is the result of parsing the symbol.
-- 
-- There is a parsing function corresponding to each field
-- accessor. For example, for the field with accessor dvBoolVal,
-- there is parsing function pBoolVal.
data Derivs = Derivs {
               -- Top level
               dvInputString    :: Result Types,
               -- Commands
               dvIfThenElse     :: Result (),
               dvWhileDo        :: Result (),
               dvSequence       :: Result (),
               dvSkip           :: Result (),
               -- Boolean
               dvBoolVal        :: Result Bool,
               dvRelExpr        :: Result Bool,
               -- Arithmetic
               dvAdditive       :: Result Int,
               dvMultitive      :: Result Int,
               dvPrimary        :: Result Int,
               dvInteger        :: Result Int,
               dvMultipleDigits :: Result (Int, Int),
               dvSingleDigit    :: Result Int,
               -- Lexical analysis
               dvKeyword        :: Result String,
               dvMultipleChars  :: Result [Char],
               dvSymbol         :: Result Char,
               dvWhitespace     :: Result (),
               -- Raw input
               dvChar :: Result Char
               }

-- The result of parsing a Derivs instance. In the case of a successful
-- Parse, the Parsed constructor takes a value and a Derivs instance
-- that provides a link to the next column in the parsing matrix. If a
-- parse fails, NoParse is used.
data Result v = Parsed v Derivs
              | NoParse

-- The three data types of the language
data Types = Command
           | BoolExp Bool
           | IntExp Int

-- A Parser is constructed with a parsing function that accepts a
-- Derivs instance, representing a column of the parsing matrix, and
-- outputs a Result, indicating the result of parsing the input column.
newtype Parser v = Parser (Derivs -> Result v)

-- Parser will only be used in monadic context, so fmap is equivalent to liftM
instance Functor Parser where
  fmap = liftM

-- Default Applicative function implementations suffice for Parser
instance Applicative Parser where
  pure  = return
  (<*>) = ap

-- Monadic parser that chains the results of parsing functions
-- The result of each parsing function iterates through the
-- columns of the parsing matrix by one or more characters.
-- From Ford listing
instance Monad Parser where
   (Parser p) >>= f = Parser pre
      where pre d = post (p d)
            post (Parsed v d') = p' d'
               where Parser p' = f v
            post NoParse = NoParse
   return x = Parser (Parsed x)

-- MonadFail forcibly exiting parse in rare circumstances
instance MonadFail Parser where
   fail _ = Parser (const NoParse)

-- Choice operator for Parser
-- If first parser succeeds, use its result
-- If first parser fails (i.e. evaluates to NoParse), try second parser
-- If both succeed, use first result
-- From Ford listing
(<|>) :: Parser v -> Parser v -> Parser v
(Parser p1) <|> (Parser p2) = Parser pre
   where pre d = post d (p1 d)
         post d NoParse = p2 d
         post d r = r


-- Functions for lexical analysis
---------------------------------

-- List of allowed keywords
keywordList :: [String]
keywordList = ["true", "false", "if", "then", "else", "fi",
               "while", "do", "od", "seq", "qes", ";", "skip"]

-- List of allowed operators and symbols
symbolList :: String
symbolList = "+-*/%()<>;"

-- Generate a list of allowed characters in a keyword
uniqueChars :: [Char]
uniqueChars = aux keywordList []
  where aux []     acc = acc
        aux (l:ls) acc = aux ls (nub (l ++ acc))

-- Parse a single character of a string
anyChar :: Parser Char
anyChar = Parser dvChar

-- Check if any of the parsed characters belong to the given container
oneOf :: Foldable t => t Char -> Parser Char
oneOf chs = sat anyChar (`elem` chs)

-- Check if provided parser satisfies the input predicate
sat :: Parser v -> (v -> Bool) -> Parser v
sat (Parser p) predicate = Parser parse
      where parse dvs = check dvs (p dvs)
            check dvs result@(Parsed val _) =
               if predicate val then result
               else NoParse
            check dvs none = none

-- Check if the given string is a keyword
keyword :: String -> Parser String
keyword s =
   do Parser dvWhitespace
      s' <- Parser dvKeyword
      if s' == s then return s
                 else fail []

-- Consume characters until the desired keyword is encountered
skipUntil :: String -> Parser String
skipUntil s =
   do keyword s
      return s
   <|>
   do c <- Parser dvChar
      skipUntil s

-- Gather characters that appear before keyword
collectUntil :: String -> Parser String
collectUntil s = aux s ""
   where
   aux kw acc =
      do keyword kw
         return acc
      <|>
      do c <- Parser dvChar
         aux kw (acc ++ [c])

-- Check if the next non-whitespace character matches the input character
symbol :: Char -> Parser Char
symbol c =
   do Parser dvWhitespace
      c' <- Parser dvSymbol
      if c' == c then return c
                 else fail []

-- Check if the given character is a whitespace character
space :: Parser Char
space = sat anyChar isSpace

-- Check if the given character is an allowed numerical digit character
digit :: Parser Char
digit = sat anyChar isDigit