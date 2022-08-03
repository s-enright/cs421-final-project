# Final Project
*CS 421*

*Summer 2022*

Sean Enright

August 4, 2022


# Overview
```
Describe the motivation, goals, and broad accomplishments of your project in general terms.
```
I have implemented a packrat parser for a simple imperative programming language in Haskell, based on Bryan Ford's 2002 paper, "Packrat parsing: simple, powerful, lazy, linear time".

Packrat parsing is a method of implementing parsers that run in linear time with backtracking and unlimited lookahead for LL(n) or LR(n) grammars, among others. Its backtracking and lookeahead allows a few difficult parsing problems to be handled with relative ease, including rules for the longest match, followed-by and not-followed by. It also allows for simple integration of lexical analysis, so packrat parsers typically do not have separate lexing steps, but rather interleave lexing and parsing.

These benefits come at the expense of memory consumption, which is significantly higher in packrat parsers than typical parsers for LL(n) and LR(n) grammars. This limitation prevented this style of parsing, first described in the 1970s, from gaining prominence unitl the early 2000s, when hardware and optimizing compilers mades these downsides an acceptable tradeoff for certain applications.

Ford describes a packrat parser implementation through a grammar for a trivial language capable of handling basic arithmetic expressions. I have extended this grammar to express a variant of an imperative programming language [3], following Ford's outline. This programming language adds boolean expressions and command statements to demonstrate the application of packrat parsing to different data types and control structures.

My implementation uses monadic combinators for clarity and to reduce the amount of code.



# Implementation

## Major Tasks and Capabilities

### Grammar

## Components of the Code


## Status of the Project

### Original Proposal
I will implement a parser for a toy language using packrat parsing, as described in the paper listed at the bottom of this message. I will be using Haskell as my language of choice, so I will employ a monadic approach to the parser's implementation.

I will provide a suite of tests that list common use cases for packrat parsing, as well as cases where it fails.

Additionally, to demonstrate the advantages and shortcomings of packrat parsing, I will benchmark its performance against another type of parser, either hand-written, or included in a library, depending on how time consuming it is.

My planned implementation schedule will divide the month of July as follows:
• Week 1: Organize project and begin implementation of parsers.
• Week 2: Finalize implementation of parsers.
• Week 3: Introduce test suite and performance comparison.
• Week 4: Summarize results, package source code and report.

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
1. Bryan Ford. *Packrat parsing: simple, powerful, lazy, linear time, functional pearl*. SIGPLAN Not. 37(9):36–47. Sept 2002. https://doi.org/10.1145/583852.581483
   
2. Bryan Ford. *Packet parsing: a practical linear-time algorithm with backtracking*. Master's Thesis, MIT. 2002. https://dspace.mit.edu/handle/1721.1/87310
   
3. Nate Foster. *Lecture #6: The IMP Language*. CS 4110, Programming Languages and Logics, Cornell. 2012. https://www.cs.cornell.edu/courses/cs4110/2012fa/lectures/lecture06.pdf

*Note: Documents for these references are included in /final-project/doc/*