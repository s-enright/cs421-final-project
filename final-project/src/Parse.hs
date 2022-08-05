module Parse where

--import Control.Applicative ( Alternative((<|>), empty) )
import Core
import Control.Monad (when, unless)
import Data.Char (digitToInt)


-- The main parsing function. This is the only entry into the key
-- data structure of the packrat parser by way of an input string.
-- It is the most important parsing function of the parser, as it
-- constructs the parsing result matrix and ties together the many
-- parsing functions of the grammar.
--
-- It takes an input string and returns a single column of a parsing
-- result matrix, a Derivs instance, and also recursively iterates
-- over the string one character at a time, creating and linking to 
-- Derivs instances, until the end of the string is reached and the
-- matrix is terminated.
-- 
-- The Derivs constructor is used to specify the many parsing
-- functions that will be used in the language. Each function has
-- one parameter, which references the encompassing Derivs instance,
-- producing the results that will be passed to subsequent parsing
-- functions. Since Haskell is lazily evaluated, many parsing
-- functions will not actually be referenced in a given column.
parse :: String -> Derivs
parse s = d where
   d      = Derivs instr
                   ite wd seq skip
                   boolv relex
                   add mult prim int
                   muldig sindig kwd mulchr sym spc chr
   -- Top level
   instr = pInputString d
   -- Commands
   ite    = pIfThenElse d
   wd     = pWhileDo d
   seq    = pSequence d
   skip   = pSkip d
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


-- A parser of commands
command :: Parser ()
command = Parser pCommand

-- A parser of boolean expressions
boolExp :: Parser Bool
boolExp = Parser pBoolExp

-- A parser of arithmetic expressions
arithExp :: Parser Int
arithExp = Parser pArithExp

-- Entry into parsing functions for each data type

-- Parse an arithmetic expression
pArithExp :: Derivs -> Result Int
pArithExp = pAdditive

-- Parse a Boolean expression
pBoolExp :: Derivs -> Result Bool
pBoolExp = pBoolVal

-- Parse a command
pCommand :: Derivs -> Result ()
pCommand = pIfThenElse


-- Main entry into packrat parser.
-- The input must belong to one of three fundmental types.
pInputString :: Derivs -> Result Types
Parser pInputString =
   do c <- command
      return Command
   <|>
   do b <- boolExp
      return (BoolExp b)
   <|>
   do a <- arithExp
      return (IntExp a)


-- Commands
-------------------------

-- Parse an if-then-else statement. If the "if" expression
-- evaluates to true, the "then" statement is executed.
-- Otherwise, the "else" statement is executed.
-- e.g. "if [b] then [c0] else [c1] fi"
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
-- e.g. "do [c] while [b] od"
-- Since the "do" command and "while" expression are evaluated
-- repeatedly, the strings representing each are collected and
-- then parsed until the "while" expression evaluates false.
pWhileDo :: Derivs -> Result ()
Parser pWhileDo =
   do keyword "do"
      s_do <- collectUntil "while"
      b_while <- collectUntil "od"
      evalWhile s_do b_while
      return ()
   <|>
   do Parser dvSequence

-- Parse a sequence of commands
-- e.g. "seq [c0] ; [c1] qes"
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
-- e.g. "skip"
pSkip :: Derivs -> Result ()
Parser pSkip =
   do keyword "skip"
      return ()


-- Boolean Expressions
-------------------------

-- Parse an explicitly given boolean value
-- Returns a boolean representing the value
-- e.g. "true", "false"
pBoolVal :: Derivs -> Result Bool
Parser pBoolVal =
   do keyword "true"
      return True
   <|>
   do keyword "false"
      return False
   <|>
   do Parser dvRelExpr

-- Parse a comparison of arithmetic values, returning a boolean
-- e.g. "2>3" = false
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


-- Arithmetic Expressions
-------------------------

-- Parse an expression with addition or subtraction
-- Both operators are right-associative.
-- e.g. "1+2" = 3, "7-5" = 2
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
-- All three operators are right-associative.
-- e.g. "1*2" = 2, "12/3" = 4, "19%13" = 6
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
-- The parser within the parentheses is the highest-precedence
-- parser, so this expression is also of high precedence.
-- e.g. "(2+3)" = 5
pPrimary :: Derivs -> Result Int
Parser pPrimary =
   do symbol '('
      vleft <- Parser dvAdditive
      symbol ')'
      return vleft
   <|>
   do Parser dvInteger

-- Parse an possibly-negative, possibly multiple-digit integer
-- e.g. "1" = 1, "-2" = -2, "39" = 39
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


-- Functions to assist parsing, but not explicitly part of the grammar
-----------------------------------------------------------------------

-- Parse an operator followed by optional whitespace
pKeyword :: Derivs -> Result String
Parser pKeyword =
   do s <- Parser dvMultipleChars
      if s `elem` keywordList then return s
      else fail []

-- Parse a character and if it belongs to the list of allowed keyword characters,
-- recursively add it to the result of this function. This will build a possible
-- keyword one character at a time.
pMultipleChars :: Derivs -> Result [Char]
Parser pMultipleChars =
   do c <- oneOf uniqueChars
      s <- Parser dvMultipleChars
      return (c:s)
   <|>
   do return []

-- Parse an allowed character symbols
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

-- Continuously parse a command string until a boolean expression string and evaluates false
evalWhile :: String -> String -> Parser ()
evalWhile doStr whileStr =
   do case dvInputString (parse doStr) of
        Parsed ty de -> return ()
        NoParse -> fail []
      case dvInputString (parse whileStr) of
        Parsed (BoolExp b) de -> when b $ evalWhile doStr whileStr
        _ -> fail []