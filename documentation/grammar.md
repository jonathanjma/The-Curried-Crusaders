# Language Grammar (BNF)

## First for your convenience, examples of valid statements to parse with ustove:
Note: Not all code is logical but it shows parsing abilities with AST.


`let x cook 10 in x+4`


`if y then 1 else 0`


`let p cook PIE * 10 in (if p then 1 else 0)`


`curry n cook let a cook curry n cook n + 1 in n`


\<integers> ::= (`0..9`)\* \
\<character> ::= \<a single ASCII character>
\<letter> ::= \<a single ASCII character that is not an integer>
\<bop> ::= `+` | `*` | `fk`

## Raw (primitive) values

\<cal> ::= \<integers> 

a valid cal must be between −2^30 and 2^30−1 

\<joul> ::= \<integers> | \<integers>.\<integers> \
\<bool> ::= `true` | `false` \
\<rcp> ::= `"` (\<character>)* `"`\
\<function> ::= `curry` \<rcp> `cook` \<expr> \
\<identifier> ::= \<letter> (\<character>)*

## Expression types


\<expr> ::= \<cal> | \<joul> | \<rcp> | \<bool> | \<function> | \<idenfitier> | \<let expression> | \<ternary expression> | \<binop> | \<function_app> 

\<binop> ::= \<expr> \<bop> \<expr>

\<let expression> ::= `let` \<identifier> `cook` \<expr> `in` \<expr> 

`let n cook e1 in e2`
- static semantics
  - evaluate e to a value v
  - v:t if and only if n:t
- dynamic semantics
  - evaluate e1 to a value v1
  - bind identifier n to value v1 in e2
  - evaluate e2 to a value v2
  - the value of the let_expr is v2

\<ternary expression> ::= `if` \<expr> `then` \<expr> `else` \<expr> 

`if e1 then e2 else e3`
- static semantics
  - e1 evaluates to either true or false
  - evaluate e1 to a value v1
  - evaluate e2 to a value v2
  - e1: 'a iff e2: 'a
- dynamic semantics
  - evaluate `e1` to a value `v1`
  - evaluate `e2` to a value `v2`
  - evaluate `e3` to a value `v3`
  - if `v1` is true, then `if e1 then e2 else e3` evaluates to `v2`
  - if `v2` is false, then if `e1 then e2 else e3` evaluates to `v3`


\<function_app> ::= \<expr> \<expr>

`e1 e2`
- static semantics
  - e1 evaluates to a function
  - if e1 : t1 -> t2
    - and e2 : t1
    - then e1 e2 : t2

- dynamic semantics
  - evaluate e1 to a value f1
  - evalute e2 to a value v2
  - evalute f1 v2 using the the definition of f



# Not implemented

\<unit> ::= `unit()`

\<bop> ::= `+` | `-` | `*` | `/` | `fk` | `|` | `&` | `=` | `bs`

- fk is short for fork, the bitwise XOR operator in our language.
- bs is short for brussel sprouts, it is the not equals operator.

\<bowl> ::= `[` \<expr>\* `,` \<expr>* `]` 



\<if expression> ::= `if` \<expr> `then` \<expr>

`if p then e`
- static semantics
  - `p` evaluates to either true or false
  - `e` evaluates to unit
- dynamic semantics
  - evaluate p to a value v
  - if v is true, then evaluate e to a value unit
  - if p then e1 evaluates to unit


\<module_decl> ::= `shelf` \<rcp> `build` \<decl>\* `end`
