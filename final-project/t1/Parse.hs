module Parse where

import Control.Applicative hiding ((<|>))
import Control.Monad (liftM, ap)
import Debug.Trace ( traceM )
import Data.Char ( isSpace, isDigit, digitToInt )

data Derivs = Derivs {
               -- Expressions
               dvAdditive :: Result Int,
               dvMultitive :: Result Int,
               dvPrimary :: Result Int,
               dvInteger :: Result Int,
               dvMultipleDigits :: Result (Int, Int),
               dvSingleDigit :: Result Int,

               -- Lexical tokens
               dvSymbol :: Result Char,
               dvWhitespace :: Result (),

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

--instance Alternative Parser where
--   empty = NoParse

(<|>) :: Parser v -> Parser v -> Parser v
(Parser p1) <|> (Parser p2) = Parser pre
   where pre d = post d (p1 d)
         post d NoParse = p2 d
         post d r = r


-- Main parse function
-- Create a result matrix for an input string
parse :: String -> Derivs
parse s = d where
   d      = Derivs add mult prim int muldig sindig sym spc chr
   add    = pAdditive d
   mult   = pMultitive d
   prim   = pPrimary d
   int    = pInteger d
   muldig = pMultipleDigits d
   sindig = pSingleDigit d
   sym    = pSymbol d
   spc    = pWhitespace d
   chr    = case s of
               (c:s') -> Parsed c (parse s')
               []     -> NoParse


-- vvv Begin parsing functions for non-terminals

pAdditive :: Derivs -> Result Int
Parser pAdditive =
   do traceM "Starting additive"
      Parser dvWhitespace
      vleft <- Parser dvMultitive
      traceM "Before plus"
      symbol '+'
      traceM "After plus"
      vright <- Parser dvAdditive
      return $ vleft + vright
   <|> do Parser dvMultitive

pMultitive :: Derivs -> Result Int
Parser pMultitive =
   do traceM "Starting multitive"
      vleft <- Parser dvPrimary
      symbol '*'
      vright <- Parser dvMultitive
      return $ vleft * vright
   <|> do Parser dvPrimary

pPrimary :: Derivs -> Result Int
Parser pPrimary =
   do traceM "starting pPrimary"
      symbol '('
      traceM "after lparen"
      vleft <- Parser dvAdditive
      traceM "after additive"
      symbol ')'
      traceM "after rparen"
      return vleft
   <|> do Parser dvInteger

pInteger :: Derivs -> Result Int
Parser pInteger =
   do (v,n) <- Parser dvMultipleDigits
      Parser dvWhitespace
      return v

pMultipleDigits :: Derivs -> Result (Int, Int)
Parser pMultipleDigits =
   do v <- Parser dvSingleDigit
      (v',n) <- Parser dvMultipleDigits
      return (v * 10^n + v', n + 1)
   <|>
   do v <- Parser dvSingleDigit
      return (v, 1)

pSingleDigit :: Derivs -> Result Int
Parser pSingleDigit =
   do digitToInt <$> digit


-- ^^^ End parsing functions for non-terminals

-- vvv Begin non-terminal parsing functions

-- From Ford paper
-- Parse an operator followed by optional whitespace
pSymbol :: Derivs -> Result Char
Parser pSymbol =
   do c <- oneOf "+-*/%()0123456789"
      Parser dvWhitespace
      return c

-- Parse zero or more whitespace characters
pWhitespace :: Derivs -> Result ()
Parser pWhitespace =
   do space
      Parser dvWhitespace
      return ()
   <|> do return ()

-- ^^^ End non-terminal parsing functions


-- Helper functions for lexical analysis
anyChar :: Parser Char
anyChar = Parser dvChar

oneOf :: Foldable t => t Char -> Parser Char
oneOf chs = sat anyChar (`elem` chs)

sat :: Parser v -> (v -> Bool) -> Parser v
sat (Parser p) predicate = Parser parse
      where parse dvs = check dvs (p dvs)
            check dvs result@(Parsed val rem) =
               if predicate val then result
               else NoParse
            check dvs none = none

symbol :: Char -> Parser Char
symbol c =
   do c' <- Parser dvSymbol
      if c' == c then return c
         else fail []

space :: Parser Char
space = sat anyChar isSpace

digit :: Parser Char
digit = sat anyChar isDigit