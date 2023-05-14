### Vision

Our vision is to make a fully-featured programming language centered around the ideas of cooking. The language will be a functional language similar to OCaml, in that almost every sytactic construct will be a type of expression that reduces to a value.

It will have access to basic types, such as int (calorie), float (joule), string (recipe), boolean (bool), and list (bowl).

It will also have the core functionality of the functional paradigm, which is the function and various syntactic sugars for manipulating them. This includes let expressions, anonymous function, if statements, binary operators, and evaluation of expressions.

We also implemented a feature that allows `.icook` files to be parsed and evaluated.

We have cooking-styled syntax and lots of fun cooking puns for the client to discover.

Once our language is further in the stages of development, we want to use it to implement a simple game involving the programming language. Our ultimate goal is to successfully create a programming language that is usable and use it to implement a meaningful creation.

### Progress Summary

After the team finished reading chapter 9 of the OCaml textbook, we started by planning out the grammar of our programming language. This included deciding which types, operators, and control structures our language would have, their syntax, and how we would name them. After that, we split our language's grammar into 4 parts so each person could work on translating our ideas into BNF form. After formalizing our grammar, the next step was to start implementing our programming language. We decided to start by working on the lexer and parser for the grammar. Currently, the parser/lexer supports integers, floats, strings, booleans, let expressions, ternary (if) statements, function declarations, and function applications. Finally we worked on creating tests, a pretty printer for the AST to help with debugging, and a interactive REPL called ustove that can parse experssions and show the pretty printed AST. After ensuring that our parsing was functional for all of these constructs, we worked on evaluating these expressions using the environment model. After implementing evaluation, we created a game called MealGuess. This game is a fun programming challenge, as it challenges the user to guess the evaluation of iCook expressions. It provides value to the user by helping them understand program evaluations with different difficulties and the capability of printing the AST. Finally, we created a program parser that allows users to run custom iCook programs
that they wrote.

### Activity Breakdown

Zach Seidner

- Implement starter code + calculator: 3 hours
- Compile and formalize documentation: 1.5 hours
- Continue implementing binary operators: 4 hours
- Add testing for binary operators and let expressions: 2 hours
- Assist with MealGuess and documentations: 2 hours
- Implement anonymous function evaluation: 2 hours

Alex Kozik

- Wrote up basic grammar with semantics: 1.5 hrs
- Implement pretty printer: 3 hours
- Implement let, ternary, function, and function application expression parsing: 3.5 hrs
- Implement random testing for int, float, and binop parsing: 2 hrs
- Implement let expression evaluation: 2 hours
- Implement testing for various expression types: 3 hours
- Implement file reading: 1 hour


Jonathan Ma

- Implement primitive types + grammar: 2 hours
- Improve error handling + research list implementation: 2 hours
- Implement list parsing: 2 hours
- Implement program parsing: 3 hours
- Implement random let definition property testing: 3 hours


Alex Wang

- Implement ustove REPL: 1.5 hours
- Debugged string parsing implementation: 1 hour
- Implemented testing for specific expressions: 4 hours
- Created MealGuess: 6 hours
- Documentation: 1 hour


### Productivity Analysis

Our team was mostly productive and was able to complete most of what we planned to do during the sprints. As a result, we felt that the estimates of what we could accomplish were pretty accurate. We were able to meet every week, adjusting our goals when necessary, and made good progress in each meeting. Due to differences in work that we had every week, our progress differed slightly between weeks, but we tried to make as much progress as possible when we had the time.
