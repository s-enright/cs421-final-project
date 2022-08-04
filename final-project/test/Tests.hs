module Tests where

import Core
import Parse

allTests :: [([Bool], String)]
allTests = [ (tests_always_pass, "Always passes")
           ]

tests_always_pass :: [Bool]
tests_always_pass = [ True
              ]

