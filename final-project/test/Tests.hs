module Tests where

import Core ( Derivs, Result(Parsed) )
import Parse ( parse, pArithExp, pBoolExp, pCommand )


-- Evaluate an expression and return the unpackaged result,
-- ignoring any unparsed remainder.           
runParser :: (Derivs -> Result v) -> String -> v
runParser p s = case p (parse s) of
                     Parsed v rem -> v
                     _ -> error "Parse error"

-- List of all tests and their titles
allTests :: [([Bool], String)]
allTests = [(testParserResult pArithExp inputInt, "Arithmetic expressions: Signed integers")
           ,(testParserResult pArithExp inputAddSub, "Arithmetic expressions: Addition and subtraction")
           ,(testParserResult pArithExp inputMult, "Arithmetic expressions: Multiplication")
           ,(testParserResult pArithExp inputDiv, "Arithmetic expressions: Division")
           ,(testParserResult pArithExp inputMod, "Arithmetic expressions: Modulo")
           ,(testParserResult pArithExp inputArith, "Arithmetic expressions: Combined operators")
           ,(testParserResult pBoolExp inputBool, "Boolean expressions: Boolean expressions and arithmetic comparison")
           ,(testParserResult pCommand inputCommand, "Commands: Successful execution of commands")
           ,(testParserResult pArithExp inputArithWhitespace, "Whitespace: Arithmetic expressions")
           ,(testParserResult pBoolExp inputBoolWhitespace, "Whitespace: Boolean expressions")
           ,(testParserResult pCommand inputCommandWhitespace, "Whitespace: Commands")
           ]

inputInt =  [("0", 0)
            ,("1", 1)
            ,("23", 23)
            ,("123456", 123456)
            ,("-0", 0)
            ,("-1", -1)
            ,("-23", -23)
            ,("-123456", -123456)
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

inputBool = [("true", True)
            ,("false", False)
            ,("34>12", True)
            ,("34<12", False)
            ,("34>-12", True)
            ,("34<-12", False)
            ,("-34>12", False)
            ,("-34<12", True)
            ,("-34<-12", True)
            ,("-34>-12", False)
            ,("44>44", False)
            ,("55<55", False)
            ]

inputCommand = [("skip", ())
               ,("if 3>1 then skip else skip fi", ())
               ,("if 3<1 then skip else skip fi", ())
               ,("if true then skip else skip fi", ())
               ,("if false then skip else skip fi", ())
               ,("do skip while 3<1 od", ())
               ,("do skip while false od", ())
               ,("seq skip ; skip qes", ())
               ,("if 3>1 then do skip while 3<1 od else seq skip ; skip qes fi", ())
               ,("if 3<1 then do skip while 3<1 od else seq skip ; skip qes fi", ())
               ,("do seq if 3>1 then skip else skip fi ; if 3<1 then skip else skip fi qes while 3<1 od", ())
               ]

inputArithWhitespace = [("   1", 1)
                       ,("2   ", 2)
                       ,("3    ", 3)
                       ,("     4    ", 4)
                       ,("    1+2", 3)
                       ,("1+2    ", 3)
                       ,("   1   +2    ", 3)
                       ,("   1+   2   ", 3)
                       ,("  1   +  2  ", 3)
                       ,("(  5+6)*7", 77)
                       ,("(   5+6 )*7", 77)
                       ,("(    5+6   )   *7", 77)
                       ,("  (     5+  6   )  *7", 77)
                       ,("     (12+34)*56+(77%8)-3*(162/-6)%-25", 2587)
                       ,("(12+34)*56+(77%8)-3*(162/-6)%-25  ", 2587)
                       ,("   (   12  +   34   )   *   56   +   (   77   %   8   )   -3   *   (   162   /   -6   )   %   -   25", 2587)
                       ,("          (12+34)*56+(77%8)-3*(162/-6)%-25        ", 2587)
                       ,("(12+   34)*56+(77%8)-3*(162/-6)%-25", 2587)
                       ,("(12   +34)*56+(77%8)-3*(162/-6)%-25", 2587)
                       ,("(12+34)*56+(77%8)-3*(162/-6)%-25", 2587)
                       ]

inputBoolWhitespace = [("   true", True)
                      ,("true   ", True)
                      ,("false  ", False)
                      ,("    false", False)
                      ,("  34>12", True)
                      ,("34>12    ", True)
                      ,("  34>12    ", True)
                      ]

inputCommandWhitespace= [("    skip", ())
                        ,("skip    ", ())
                        ,("   skip  ", ())
                        ,(" do seq if 3 > 1 then skip else skip fi ; if 3 < 1 then skip else skip fi qes while 3 < 1 od ", ())
                        ,("do seq if 3>1 then skip else skip fi ; if 3<1 then skip else skip fi qes while 3<1 od", ())
                        ,("    do   seq    if   3  >  1   then   skip    else   skip   fi   ;  if   3   <   1   then    skip   else    skip   fi  qes    while  3  <  1  od   ", ())
                        ]

-- Test a list of input strings against their expected values by running
-- a parser on each of them and reporting the result in a list of Booleans
testParserResult :: Eq a => (Derivs -> Result a) -> [(String, a)] -> [Bool]
testParserResult parser inputList = 
   let testPair (inputStr, correctAnswer) =
         runParser parser inputStr == correctAnswer
   in map testPair inputList