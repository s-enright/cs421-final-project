module Tests where

import Core ( Derivs, Result(Parsed) )
import Parse ( parse, pArithExp )


-- Evaluate an expression and return the unpackaged result,
-- ignoring any unparsed remainder.           
runParser :: (Derivs -> Result v) -> String -> v
runParser p s = case p (parse s) of
                     Parsed v rem -> v
                     _ -> error "Parse error"

-- List of all tests and their titles
allTests :: [([Bool], String)]
allTests = [(testArithExp inputAddSub, "Arithmetic expressions: Addition and Subtraction")
           ,(testArithExp inputMult, "Arithmetic expressions: Multiplication")
           ,(testArithExp inputDiv, "Arithmetic expressions: Division")
           ,(testArithExp inputMod, "Arithmetic expressions: Modulo")
           ,(testArithExp inputArith, "Arithmetic expressions: Combined operators")
           ]

inputAddSub = [("0+0", 0)
              ,("0+1", 1)
              ,("1+0", 1)
              ,("1+2", 3)
              ,("3-4", -1)
              ,("5+6", 11)
              ,("67+89", 156)
              ,("0-0", 0)
              ,("0-1", -1)
              ,("1-0", 1)
              ,("1-1", 0)
              ,("42-56", -14)
              ,("45--56", 101)
              ,("-42+56", 14)
              ,("-39-87", -126)
              ]

inputMult = [("0*0", 0)
            ,("0*9", 0)
            ,("9*0", 0)
            ,("3*3", 9)
            ,("-4*-5", 20)
            ,("-45*-56", 2520)
            ,("67*89", 5963)
            ,("123*-456", -56088)
            ,("-789*1234", -973626)
            ,("1*123456789", 123456789)
            ]

inputDiv =  [("0/9", 0)
            ,("0/99", 0)
            ,("9/3", 3)
            ,("828/12", 69)
            ,("1/2", 0)
            ,("2/3", 0)
            ,("1/2", 0)
            ,("2/3", 0)
            ,("-1/2", -1)
            ,("-2/3", -1)
            ,("-1/2", -1)
            ,("1/-2", -1)
            ,("2/-3", -1)
            ,("1/-2", -1)
            ,("-1/-2", 0)
            ,("-2/-3", 0)
            ,("-1/-2", 0)
            ,("2/3", 0)
            ,("-3652305089/-421", 8675309)
            ,("-12033/9", -1337)
            ,("17682/-42", -421)
            ,("123456789/1", 123456789)
            ]

inputMod =  [("3%12", 3)
            ,("17%1", 0)
            ,("3%17", 3)
            ,("56%17", 5)
            ,("33%-12", -3)
            ,("-405%-360", -45)
            ]

inputArith = [("0+1+2+3+4+5+6", 21)
             ,("((((((0-1)-2)-3)-4)-5)-6)", -21)
             ,("1*2*3*4*5*6", 720)
             ,("(((((1048576/128)/32)/8)/2)/1)", 16) -- division is right-associative
             ,("2*(3+4)", 14)
             ,("(5+6)*7", 77)
             ,("(12+34)*56+(77%8)", 2581)
             ,("3*(162/-6)%-25", -6)
             ,("(12+34)*56+(77%8)-3*(162/-6)%-25", 2587)
             ,("((12+34)*56+(77%8)-3*(162/-6)%-25)%2545", 42)             
             ]

-- Test a list of arithmetic input strings against their expected values
testArithExp :: [(String, Int)] -> [Bool]
testArithExp inputList = 
   let testPair (inputStr, correctAnswer) =
         runParser pArithExp inputStr == correctAnswer
   in map testPair inputList