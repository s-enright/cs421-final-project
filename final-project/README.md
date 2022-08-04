# Final Project
**CS 421, Summer 2022**

Sean Enright

August 4, 2022


# Overview
I have implemented a packrat parser for a simple imperative programming language in Haskell, based on Bryan Ford's 2002 paper, "Packrat parsing: simple, powerful, lazy, linear time".

Packrat parsing is a method of implementing parsers that run in linear time with backtracking and unlimited lookahead for LL(n) or LR(n) grammars, among others. Its backtracking and lookahead allow a few difficult parsing problems to be handled with relative ease, including rules for the longest match, followed-by and not-followed by. It also allows for simple integration of lexical analysis, so packrat parsers typically do not have separate lexing and parsing stages, but rather interleave them.

These benefits come at the expense of memory consumption, which is significantly higher in packrat parsers than typical parsers for LL(n) and LR(n) grammars. This limitation prevented this style of parsing, first described in the 1970s, from gaining prominence until the early 2000s, when hardware and optimizing compilers made these downsides an acceptable trade-off for certain applications.

Ford describes a packrat parser implementation through a grammar for a trivial language describing basic arithmetic expressions. I have extended this grammar to express a variant of a simple imperative programming language[2], following Ford's outline. This language adds boolean expressions and command statements to demonstrate the application of packrat parsing to different data types and control structures.

My implementation uses monadic combinators for clarity and to reduce the amount of code.

A REPL is provided to allow a user interface to the parser.


# Implementation

## Major Tasks and Capabilities

### Capabilities of the Parser
The key concept behind packrat parsing is the use of a specialized data structure, consisting of a matrix of possible results for each of the parse functions in the grammar for each of the input characters. The results of parsing each character of input are memoized, potentially allowing for greater efficiency in accessing repeated patterns. For an input string of length *n*, and *p* parsing functions, this matrix will have (*n* + 1) \* *p* cells, with the *n* + 1 term accounting for each character of the string as well as an empty string. This is potentially a very large table, so in order to efficiently make use of it, this parsing style effectively requires the use of a lazily evaluated language. For this reason, and for the potential to implement it via a monadic approach, Haskell was chosen for this project.

Parsing functions are also included for lexical analysis, including matching whitespace of varying length, matching sets of operators and keywords, and of matching numerical characters and forming them into possibly negative-valued integers.


### Grammar
To demonstrate a minimal implementation of a packrat parser, Ford introduces a trivial language described by the following grammar.

```
Additive  <- Multitive ‘+’ Additive | Multitive
Multitive <- Primary ‘*’ Multitive | Primary
Primary   <- ‘(’ Additive ‘)’ | Decimal
Decimal   <- ‘0’ | ... | ‘9’
```

This language was extended to closely approximate the IMP language[2], with a few minor modifications made to reduce ambiguity and reduce the amount of symbols for the sake of demonstration. 

In Backus-Nauer form, this grammar is described as:
```
Arithmetic Expressions
a ::= a0 "+" a1 | a0 "-" a1 | a0 "*" a1 | a0 "/" a1 | a0 "mod" a1 | i
i ::= "-" d | d
d ::= "0" | ... | "9"

Boolean Expressions
b ::= "true" | "false" | a0 "<" a1 | a0 ">" a1

Commands
c ::= "if" b "then" c0 "else" c1 "fi" | "do" c "while" b "od" | "seq" c0 ";" c1 "qes" | "skip"
```

Most notably, the grammar was extended to encompass boolean expressions and commands, increasing the expressive power of the language beyond that of a simple calculator. Commands were also given unique initial and final terminal symbols to reduce parsing errors in nested expressions. The introduction of new syntactic sets (i.e. boolean expressions and commands) required wrapper data types to allow the parser to handle combinations of different expression types or commands.

The addition of commands to the grammar required some modifications to the lexing functions to be able to capture commands evaluated repeatedly in do-while loops and in sequence. For this purpose functions were introduced to capture a string of input until a particular character or keyword is encountered, and to then parse these strings according to the loop or sequence status.

### Challenges
The initial challenge in this project was in fully comprehending the level of recursion required to implement the basic data structures and parsing functions. In addition to the main memoization matrix, which is implemented through a pair mutually recursive data structures, the main parsing function itself is doubly recursive, assigning variables that refer to itself, and to later iterations of the parsing results. These are described in detail below.

Another challenge was updating the example code to work with contemporary GHC Haskell. Although only some twenty years have passed since the publication of Ford's paper and thesis[1,3], significant alterations to the source code were necessary to make the examples functional.

## Components of the Code
The key data structures of the parser are found in \/src\/Core.hs

A column of the specialized parsing matrix described above is represented by the algebraic data type Derivs, which uses record syntax to create accessor functions that can be passed into parsing functions. Each field is of type Result, representing the result of an elementary parse.

```
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
```

The Result data type represents the result of a parsing function, which either is "NoParse", indicating the end of a line or a parsing failure, or "Parsed", which also includes the value returned by the parsing function and an instance of a Derivs data type, which provides a link to the next column of the parsing matrix.

```
data Result v = Parsed v Derivs
              | NoParse
```

These two data structures, Result and Derivs, are mutually recursive, thereby providing a link between columns of the parsing matrix. A parsing function that succeeds yields a Result, which itself contains a Derivs instance linking to the next column. This continues until either a parsing failure occurs or the input string is completely consumed.


## Status of the Project

### Original Proposal
I will implement a parser for a toy language using packrat parsing, as described in the paper listed at the bottom of this message. I will be using Haskell as my language of choice, so I will employ a monadic approach to the parser's implementation.

I will provide a suite of tests that list common use cases for packrat parsing, as well as cases where it fails.

Additionally, to demonstrate the advantages and shortcomings of packrat parsing, I will benchmark its performance against another type of parser, either hand-written, or included in a library, depending on how time consuming it is.

My planned implementation schedule will divide the month of July as follows:

* Week 1: Organize project and begin implementation of parsers.
* Week 2: Finalize implementation of parsers.
* Week 3: Introduce test suite and performance comparison.
* Week 4: Summarize results, package source code and report.

### Evaluation of Project Status
```
what works well,  what works partially,
      and and what is not implemented at all.
```





# Using the Packrat Parser

## Installation
This project uses stack for build management.

To build: `stack build`

To run the packrat REPL: `stack run`

To execute the test suite: `stack test`

## REPL
`stack run` will build and start the REPL. The result of the entered expression will be printed on the next line.

```
>1 + 2
3
```

```
>2*(3+4)
14
```

Enter `exit` or `quit` to leave the REPL.







# Tests
```
Coming up with appropriate tests is one of the most important part of  good software development. 
 Your tests should include unit tests, feature  tests, and larger test codes.
 Give a short description of the tests  used, performance results if appropriate 
 (e.g., memory consumption for  garbage collection) etc. Be sure to explain how these tests exercise the  concept(s) you've implemented.
 ```


# References
1. Bryan Ford. 2002. Packrat parsing: simple, powerful, lazy, linear time, functional pearl. *SIGPLAN Not.* 37, 9 (September 2002), 36–47. https://doi.org/10.1145/583852.581483

2. Glynn Winskel. 2001. *The Formal Semantics of Programming Languages: An Introduction*. 12-24. MIT Press.

3. Bryan Ford. 2002. Packet parsing: a practical linear-time algorithm with backtracking. Master's Thesis. MIT, Cambridge, MA. Retrieved August 2, 2022 from https://dspace.mit.edu/handle/1721.1/87310

4. Nate Foster. 2012. Lecture #6: The IMP Language. CS 4110, Programming Languages and Logics, Cornell. Retrieved August 2, 2022 from https://www.cs.cornell.edu/courses/cs4110/2012fa/lectures/lecture06.pdf

*Note: Documents for these references are included in /final-project/doc/*