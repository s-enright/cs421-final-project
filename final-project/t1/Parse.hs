module Parse where

import Control.Applicative hiding ((<|>))
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
command = Parser pSkip
boolExp :: Parser Bool
boolExp = Parser pBoolVal
arithExp :: Parser Int
arithExp = Parser pAdditive

{-
validInput :: Derivs -> Result (Parser ())
Parser validInput =
   do return command
   <|>
   do return boolExp
   <|>
   do return arithExp
-}

pSkip :: Derivs -> Result ()
Parser pSkip =
   do keyword "skip"
      traceM "skipping..."
      return ()
   <|>
   do Parser pIfThenElse

pIfThenElse :: Derivs -> Result ()
Parser pIfThenElse =
   do keyword "if"
      b <- boolExp
      keyword "then"
      -- Case 1: boolean is true
      when b $ do c <- command
                  keyword "else"
                  skipUntil "fi"
                  return ()
      -- Case 2: boolean is false
      unless b $ do skipUntil "else"
                    c <- command
                    keyword "fi"
                    return ()
   <|>
   do Parser pWhileDo

pWhileDo :: Derivs -> Result ()
Parser pWhileDo =
   do keyword "do"
      s_do <- collectUntil "while"
      --b <- boolExp
      b_while <- collectUntil "od"
      traceM $ "Do: " ++ s_do ++ ", While: " ++ b_while ++ "."
      if b_while /= "" then traceM "ok" else return ()
      --when b $ do Parser dvWhileDo
      --unless b $ do keyword "od"
      return ()
   <|>
   do Parser pSequence

pSequence :: Derivs -> Result ()
Parser pSequence =
   do keyword "do"
      c1 <- command
      symbol ';'
      c2 <- command
      keyword "od"
      return ()


-- Boolean
pBoolVal :: Derivs -> Result Bool
Parser pBoolVal =
   do keyword "true"
      return True
   <|>
   do keyword "false"
      return False
   <|>
   do Parser dvRelExpr

pRelExpr :: Derivs -> Result Bool
Parser pRelExpr =
   do traceM "Starting pRelexpr"
      vl <- arithExp
      symbol '>'
      vr <- arithExp
      return (vl > vr)
   <|>
   do vl <- arithExp
      symbol '<'
      vr <- arithExp
      return (vl < vr)

-- Arithmetic
pAdditive :: Derivs -> Result Int
Parser pAdditive =
   do traceM "Starting additive"
      vleft <- Parser dvMultitive
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
      c <- oneOf "+-*/%()<>"
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
keywordList = ["true", "false", "skip", "if", "then", "else", "fi", "while", "do", "od"]

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
            check dvs result@(Parsed val rem) =
               if predicate val then result
               else NoParse
            check dvs none = none

keyword :: String -> Parser String
keyword s =
   do Parser dvWhitespace
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
   do c' <- Parser dvSymbol
      if c' == c then return c
                 else fail []

space :: Parser Char
space = sat anyChar isSpace

digit :: Parser Char
digit = sat anyChar isDigit