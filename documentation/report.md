### Vision
Our vision is to make a fully-featured programming language centered around the ideas of cooking. We have cooking styled syntax and lots of
fun cooking puns for the client to discover. Eventually, we want to use our programming language to implement a simple text-based cooking 
game into our language. The main goal is to successfully create a programming language that is usable.

### Progress Summary
After the team finished reading chapter 9 of the OCaml textbook, we started by planning out the grammar of our programming language. 
This included deciding which types, operators, and control structures our language would have, their syntax, and how we would name them. 
After that, we split our language into 4 parts so each person could work on translating our ideas into BNF form. After formalizing our 
grammer, the next step was to start implementing our programming language. We decided to start by working on the lexer and parser for the grammar. 
Currently, the parser/lexer supports integers, floats, strings, characters, booleans, let statements, ternary (if) statements, function declarations,
and function applications. Finally we worked on creating tests, a pretty printer for the AST to help with debugging, and a interactive REPL called 
ustove that can parse experssions and show the pretty printed AST. 

### Activity Breakdown
Zach Seidner
- Implement starter code + calculator: 3 hours
- Compile and formalize documentation: 1.5 hours
 
Alex Kozik
- Wrote up basic grammar with semantics: 1.5 hrs
- Implement pretty printer: 3 hours
- Implement let, ternary, function, and function application expression parsing: 3.5 hrs

Jonathan Ma
- Implement primitive types + grammar: 2 hours
- Improve error handling + research list implementation: 2 hours

Alex Wang
- Implement ustove REPL: 1.5 hours

### Productivity Analysis
Our team was mostly productive and was able to complete most of what we planned to do during the sprints. As a result, we felt that the estimates
of what we could accomplish were pretty accurate. However, because members of the teams had prelims during this time, we were unable to implement
features such as list parsing, adding colors to the pretty printer, and fixing various expression parsing bugs.
