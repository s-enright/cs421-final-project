module Parse where

import Control.Applicative hiding ((<|>))
import Control.Monad (liftM, ap)
import Debug.Trace ( traceM )
import Data.Char ( isSpace, isDigit, digitToInt )
import Data.List (nub)

data Derivs = Derivs {
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
   d      = Derivs boolv relex add mult prim int muldig sindig kwd mulchr sym spc chr
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

-- Boolean
pBoolVal :: Derivs -> Result Bool
Parser pBoolVal =
   do v <- Parser dvKeyword
      case v of
         "true"  -> return True
         "false" -> return False
         _       -> fail []
   <|>
   do Parser dvRelExpr

Parser pRelExpr =
   do traceM "Starting pRelexpr"
      vl <- Parser dvAdditive
      symbol '>'
      vr <- Parser dvAdditive
      return (vl > vr)
   <|>
   do vl <- Parser dvAdditive
      symbol '<'
      vr <- Parser dvAdditive
      return (vl < vr)

-- Arithmetic
pAdditive :: Derivs -> Result Int
Parser pAdditive =
   do traceM "Starting additive"
      Parser dvWhitespace
      vleft <- Parser dvMultitive
      symbol '+'
      vright <- Parser dvAdditive
      return $ vleft + vright
   <|> 
   do Parser dvWhitespace
      vleft <- Parser dvMultitive
      symbol '-'
      vright <- Parser dvAdditive
      return $ vleft - vright
   <|>
   do Parser dvMultitive

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

pPrimary :: Derivs -> Result Int
Parser pPrimary =
   do symbol '('
      vleft <- Parser dvAdditive
      symbol ')'
      return vleft
   <|>
   do Parser dvInteger

pInteger :: Derivs -> Result Int
Parser pInteger =
   do symbol '-'
      (v,n) <- Parser dvMultipleDigits
      Parser dvWhitespace
      return (-v)
   <|>
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
keywords :: [String]
keywords = ["true", "false", "skip", "if", "then", "else", "while", "do"]

-- Generates a list of allowed characters in a keyword
uniqueChars :: [Char]
uniqueChars = aux keywords []
  where aux []     acc = acc
        aux (l:ls) acc = aux ls (nub (l ++ acc))

pKeyword :: Derivs -> Result String
Parser pKeyword =
   do --traceM "Starting Keyword"
      s <- Parser dvMultipleChars
      --traceM $ "s: " ++ s ++ ". end"
      if s `elem` keywords then return s
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

pSymbol :: Derivs -> Result Char
Parser pSymbol =
   do c <- oneOf "+-*/%()<>"
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