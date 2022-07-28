module Parse where
--import Control.Monad.Fail
import Control.Applicative hiding ((<|>))
import Control.Monad (liftM, ap)
import Debug.Trace

data Derivs = Derivs {
               -- Expressions
               dvAdditive :: Result Int,
               dvMultitive :: Result Int,
               dvPrimary :: Result Int,
               dvDecimal :: Result Int,

               -- Lexical tokens
               --dvDigits :: Result (Int, Int),
               --dvDigit :: Result Int,
               --dvSymbol :: Result Char,
               --dvWhitespace :: Result (),

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
   return x = Parser (\d -> Parsed x d)

instance MonadFail Parser where
   fail _ = Parser (\d -> NoParse)

--instance Alternative Parser where
--   empty = NoParse
--

(<|>) :: Parser v -> Parser v -> Parser v
(Parser p1) <|> (Parser p2) = Parser pre
   where pre d = post d (p1 d)
         post d NoParse = p2 d
         post d r = r


-- Main parse function
-- Create a result matrix for an input string
   --d = Derivs add mult prim dec digits digit sym whsp chr
parse :: String -> Derivs
parse s = d where
   d = Derivs add mult prim dec chr
   add = pAdditive d
   mult = pMultitive d
   prim = pPrimary d
   dec = pDecimal d
   {-
   digits = pDigits d
   digit = pDigit d
   sym = pSymbol d
   whsp = pWhitespace d
   chr = pChar d
   -}
   chr = case s of
           (c:s') -> Parsed c (parse s')
           []     -> NoParse


---------- Character-oriented parsers
-- look up alternative implementation?
isSpace :: Char -> Bool
isSpace c = c `elem` " \n\t\r"



{-
-- TODO: figure out this char function
char :: Derivs -> Result Char
char c = if c == '+'
             then Parsed c d
             else NoParse
-}

-- "Parser combinator" to look for a specific symbol.
-- Cannot be memoized directly because it takes a parameter,
-- but that does not matter because it is non-recursive.
{-
symbol c =
   do c' <- Parser dvSymbol
      if c' == c then return c
         else fail []
   -- <?> show c
-}

symbol c =
   do c' <- Parser dvChar
      traceM $ "symbol: " ++ [c] ++ ", stream: " ++ [c']
      if c' == c then 
         do traceM "success"
            return c'
      else fail []
   
--   do c' <- Parser dvSymbol
--     if c' == c then return c
--       else fail []

Parser pAdditive =
   do traceM "Starting additive"
      vleft <- Parser dvMultitive
      traceM "Before plus"
      symbol '+'
      traceM "After plus"
      vright <- Parser dvAdditive
      return (vleft + vright)
   <|> (do Parser dvMultitive)

Parser pMultitive =
   do vleft <- Parser dvPrimary
      symbol '*'
      vright <- Parser dvMultitive
      return (vleft * vright)
      <|> (do Parser dvPrimary)

Parser pPrimary =
   do traceM "starting pPrimary"
      symbol '('
      traceM "after lparen"
      vleft <- Parser dvAdditive
      traceM "after additive"
      symbol ')'
      traceM "after rparen"
      return vleft
      <|> (do Parser dvDecimal)

Parser pDecimal =
   do symbol '0'
      return 0
   <|>
   do symbol '1'
      return 1
   <|>
   do symbol '2'
      return 2
   <|>
   do symbol '3'
      return 3
   <|>
   do symbol '4'
      return 4
   <|>
   do symbol '5'
      return 5
   <|>
   do symbol '6'
      return 6
   <|>
   do symbol '7'
      return 7
   <|>
   do symbol '8'
      return 8
   <|>
   do symbol '9'
      return 9
   
      
Parser pDigits = undefined
Parser pDigit = undefined

-- From Ford paper
-- Parse an operator followed by optional whitespace
{-
pSymbol :: Derivs -> Result Char
pSymbol d =
   case dvChar d of
      Parsed c d' ->
         if c `elem` "+-*/%()"
         then case dvWhitespace d' of
            Parsed _ d'' -> Parsed c d''
            _ -> NoParse
         else NoParse
      _ -> NoParse

-- From Ford paper
-- Parsing functions for lexical analysis
pWhitespace :: Derivs -> Result ()
pWhitespace d =
   case dvChar d of
      Parsed c d' ->
         if isSpace c
         then pWhitespace d'
         else Parsed () d
      _ -> Parsed () d

-}
Parser pChar = undefined