module Parse where

--import Control.Applicative ( Alternative((<|>), empty) )
import Core
import Control.Monad (when, unless)
import Debug.Trace ( traceM )
import Data.Char (digitToInt)


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


-- Commands
-------------------------

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


-- Boolean Expressions
-------------------------

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

-- Arithmetic Expressions
-------------------------

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
   do case pValidInput (parse doStr) of
        Parsed ty de -> return ()
        NoParse -> fail []
      case pValidInput (parse whileStr) of
        Parsed (BoolExp b) de -> when b $ evalWhile doStr whileStr
        _ -> fail []