{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parse where

--import Control.Applicative ( Alternative((<|>), empty) )
import Control.Monad (liftM, ap, when, unless)
import Debug.Trace ( traceM )
import Data.Char ( isSpace, isDigit, digitToInt )
import Data.List (nub)
import Data.Bool (bool)

data Derivs = Derivs {
               -- Commands
               dvSkip           :: Result (),
               --dvAssign         :: Result (),
               dvSequence       :: Result (),
               dvIfThenElse     :: Result (),
               dvWhileDo        :: Result (),
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

               -- Lexical tokens
               dvKeyword        :: Result String,
               dvMultipleChars  :: Result [Char],
               dvSymbol         :: Result Char,
               dvWhitespace     :: Result (),

               -- Raw input
               dvChar :: Result Char
               }

data Result v = Parsed v Derivs
              | NoParse

newtype Parser v = Parser (Derivs -> Result v)

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

-- from Ford paper
instance Monad Parser where
   (Parser p) >>= f = Parser pre
      where pre d = post (p d)
            post (Parsed v d') = p' d'
               where Parser p' = f v
            post NoParse = NoParse
   return x = Parser (Parsed x)

instance MonadFail Parser where
   fail _ = Parser (const NoParse)

-- Alternative function
(<|>) :: Parser v -> Parser v -> Parser v
(Parser p1) <|> (Parser p2) = Parser pre
   where pre d = post d (p1 d)
         post d NoParse = p2 d
         post d r = r

{-
instance Alternative Parser where
   empty = NoParse
   (Parser l) <|> NoParse = (\d -> Parsed l Derivs)
-}

-- Main parse function
-- Create a result matrix for an input string
-- The double recursion in this function allows the result
-- matrix to lazily be evaluated while advancing through
-- the input string one character at a time.
parse :: String -> Derivs
parse s = d where
   d      = Derivs skip seq ite wd
                   boolv relex
                   add mult prim int
                   muldig sindig kwd mulchr sym spc chr
   -- Commands
   skip   = pSkip d
   seq    = pSequence d
   ite    = pIfThenElse d
   wd     = pWhileDo d
   -- Boolean
   boolv  = pBoolVal d
   relex  = pRelExpr d
   -- Arithmetic
   add    = pAdditive d
   mult   = pMultitive d
   prim   = pPrimary d
   int    = pInteger d
   -- Lexical
   muldig = pMultipleDigits d
   sindig = pSingleDigit d
   kwd    = pKeyword d
   mulchr = pMultipleChars d
   sym    = pSymbol d
   spc    = pWhitespace d
   chr    = case s of
               (c:s') -> Parsed c (parse s')
               []     -> NoParse


-- vvv Begin parsing functions for non-terminals
command :: Parser ()
command = Parser pIfThenElse
boolExp :: Parser Bool
boolExp = Parser pBoolVal
arithExp :: Parser Int
arithExp = Parser pAdditive


data Types = Command
           | BoolExp Bool
           | IntExp Int

-- Main entry into packrat parser.
-- The input must belong to one of three fundmental types.
pValidInput :: Derivs -> Result Types
Parser pValidInput =
   do c <- command
      return Command
   <|>
   do b <- boolExp
      return (BoolExp b)
   <|>
   do a <- arithExp
      return (IntExp a)


-- Parse an if-then-else statement. If the "if" expression
-- evaluates to true, the "then" statement is executed.
-- Otherwise, the "else" statement is executed.
pIfThenElse :: Derivs -> Result ()
Parser pIfThenElse =
   do keyword "if"
      b <- boolExp
      keyword "then"
      -- Case 1: boolean is true
      when b $ do cif <- command
                  keyword "else"
                  skipUntil "fi"
                  return cif
      -- Case 2: boolean is false
      unless b $ do skipUntil "else"
                    cel <- command
                    keyword "fi"
                    return cel
   <|>
   do Parser pWhileDo

-- Parse a do-while loop statement
pWhileDo :: Derivs -> Result ()
Parser pWhileDo =
   do keyword "do"
      s_do <- collectUntil "while"
      b_while <- collectUntil "od"
      traceM $ "Do: " ++ s_do ++ ", While: " ++ b_while ++ "."
      evalWhile s_do b_while
      return ()
   <|>
   do Parser pSequence

-- Parse a sequence of commands
pSequence :: Derivs -> Result ()
Parser pSequence =
   do keyword "seq"
      c1 <- command
      symbol ';'
      c2 <- command
      keyword "qes"
      return ()
   <|>
   do Parser pSkip

-- Parse an explicitly given "skip" statement
pSkip :: Derivs -> Result ()
Parser pSkip =
   do keyword "skip"
      return ()

-- Boolean
-- Parse an explicitly given boolean value
pBoolVal :: Derivs -> Result Bool
Parser pBoolVal =
   do keyword "true"
      return True
   <|>
   do keyword "false"
      return False
   <|>
   do Parser dvRelExpr

-- Parse a comparison of arithmetic values
pRelExpr :: Derivs -> Result Bool
Parser pRelExpr =
   do vl <- arithExp
      symbol '>'
      vr <- arithExp
      return (vl > vr)
   <|>
   do vl <- arithExp
      symbol '<'
      vr <- arithExp
      return (vl < vr)

-- Arithmetic
-- Parse an expression with addition or subtraction
pAdditive :: Derivs -> Result Int
Parser pAdditive =
   do vleft <- Parser dvMultitive
      symbol '+'
      vright <- Parser dvAdditive
      return $ vleft + vright
   <|>
   do vleft <- Parser dvMultitive
      symbol '-'
      vright <- Parser dvAdditive
      return $ vleft - vright
   <|>
   do Parser dvMultitive

-- Parse an expression that uses multiplication,
-- division, or a modulo operation
pMultitive :: Derivs -> Result Int
Parser pMultitive =
   do vleft <- Parser dvPrimary
      symbol '*'
      vright <- Parser dvMultitive
      return $ vleft * vright
   <|>
   do vleft <- Parser dvPrimary
      symbol '/'
      vright <- Parser dvMultitive
      case vright of
         0 -> fail "Divide by zero error."
         _ -> return $ vleft `div` vright
   <|>
   do vleft <- Parser dvPrimary
      symbol '%'
      vright <- Parser dvMultitive
      return $ vleft `mod` vright
   <|>
   do Parser dvPrimary

-- Parse an arithmetic expression within parentheses
pPrimary :: Derivs -> Result Int
Parser pPrimary =
   do symbol '('
      vleft <- Parser dvAdditive
      symbol ')'
      return vleft
   <|>
   do Parser dvInteger

-- Parse an integer, which may be negative
pInteger :: Derivs -> Result Int
Parser pInteger =
   do Parser dvWhitespace
      symbol '-'
      (v,n) <- Parser dvMultipleDigits
      Parser dvWhitespace
      return (-v)
   <|>
   do Parser dvWhitespace
      (v,n) <- Parser dvMultipleDigits
      Parser dvWhitespace
      return v

-- Accumulate an integer value by combining single-digit numbers
pMultipleDigits :: Derivs -> Result (Int, Int)
Parser pMultipleDigits =
   do v <- Parser dvSingleDigit
      (v',n) <- Parser dvMultipleDigits
      return (v * 10^n + v', n + 1)
   <|>
   do v <- Parser dvSingleDigit
      return (v, 1)

-- Parse a single-digit number
pSingleDigit :: Derivs -> Result Int
Parser pSingleDigit =
   do digitToInt <$> digit


-- ^^^ End parsing functions for non-terminals

-- vvv Begin non-terminal parsing functions

-- From Ford paper
-- Parse an operator followed by optional whitespace

pKeyword :: Derivs -> Result String
Parser pKeyword =
   do --traceM "Starting Keyword"
      s <- Parser dvMultipleChars
      --traceM $ "s: " ++ s ++ ". end"
      if s `elem` keywordList then return s
      else fail []

pMultipleChars :: Derivs -> Result [Char]
Parser pMultipleChars =
   do c <- oneOf uniqueChars
      --traceM $ "char: " ++ [c]
      s <- Parser dvMultipleChars
      --traceM (c:s)
      return (c:s)
   <|>
   do return []

-- Check for allowed character symbols
pSymbol :: Derivs -> Result Char
Parser pSymbol =
   do Parser dvWhitespace
      c <- oneOf symbolList
      Parser dvWhitespace
      return c

-- Parse zero or more whitespace characters
pWhitespace :: Derivs -> Result ()
Parser pWhitespace =
   do space
      Parser dvWhitespace
      return ()
   <|>
   do return ()

-- ^^^ End non-terminal parsing functions


-- Helper functions for lexical analysis
keywordList :: [String]
keywordList = ["true", "false", "if", "then", "else", "fi",
               "while", "do", "od", "seq", "qes", ";", "skip"]

symbolList :: String
symbolList = "+-*/%()<>;"

-- Generates a list of allowed characters in a keyword
uniqueChars :: [Char]
uniqueChars = aux keywordList []
  where aux []     acc = acc
        aux (l:ls) acc = aux ls (nub (l ++ acc))

anyChar :: Parser Char
anyChar = Parser dvChar

oneOf :: Foldable t => t Char -> Parser Char
oneOf chs = sat anyChar (`elem` chs)

sat :: Parser v -> (v -> Bool) -> Parser v
sat (Parser p) predicate = Parser parse
      where parse dvs = check dvs (p dvs)
            check dvs result@(Parsed val _) =
               if predicate val then result
               else NoParse
            check dvs none = none

keyword :: String -> Parser String
keyword s =
   do --traceM $ "  looking for keyword: " ++ s
      Parser dvWhitespace
      s' <- Parser dvKeyword
      if s' == s then return s
                 else fail []

-- Consume characters until the
-- desired keyword is encountered
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

symbol :: Char -> Parser Char
symbol c =
   do --traceM $ "  looking for symbol: " ++ [c]
      Parser dvWhitespace
      c' <- Parser dvSymbol
      if c' == c then return c
                 else fail []

space :: Parser Char
space = sat anyChar isSpace

digit :: Parser Char
digit = sat anyChar isDigit

evalWhile :: String -> String -> Parser ()
evalWhile doStr whileStr =
   do case pValidInput (parse doStr) of
        Parsed ty de -> return ()
        NoParse -> fail []
      case pValidInput (parse whileStr) of
        Parsed (BoolExp b) de -> when b $ evalWhile doStr whileStr
        _ -> fail []