# Final Project
*CS 421*
*Summer 2022*

Sean Enright
August 4, 2022

# Overview
```
Describe the motivation, goals, and broad accomplishments of your project in general terms.
```



# Implementation
```
A brief description of the important aspects of your implementation, in terms of
  (a) the major tasks or capabilities of your code;
  (b) components of the code;
  (d) status of the project – what works well,  what works partially,
      and and what is not implemented at all. You MUST  compare these
      with your original proposed goals in the project proposal.
```

## Capabilities

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


# Installation
This project uses stack for build management.

To build: `stack build`
To run the packrat repl: `stack run`
To execute the test suite: `stack test`

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