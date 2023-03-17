# The Curried Crusaders

## OCaml Project Team sponsored by camels 🐫

### Members:

- Zachary Seidner (zes4)
- Jonathan Ma (jjm498)
- Alex Wang (ayw29)
- Alex Kozik (ajk333)

# Language BNF

\<integers> ::= (`0..9`)\* \
\<character> ::= \<a single ASCII character>

## Raw (primitive) types

\<cal> ::= \<integers> \
\<joul> ::= \<integers> | \<integers>.\<integers> \
\<ing> ::= \<character\> \
\<bop> ::= `+` | `-` | `*` | `/` | `fk` | `|` | `&` | `=` | `bs` \
\<bool> ::= `true` | `false` \
\<unit> ::= `unit()`

- fk is short for fork, the bitwise XOR operator in our language.
- bs is short for brussel sprouts, it is the not equals operator.

## Cooked (non-primitive) types

\<bowl> ::= `[` \<expr>\* `,` \<expr>* `]` \
\<binop> ::= \<bop> $*$ ( \<cal> | \<joul>) $*$ (\<cal> | \<joul>) \
\<expr> ::= \<cal> | \<joul> | \<rcp> | \<ing> | \<bool> | \<bowl> | \<binop> \
\<statement> ::= \<let statement> | \<ternary statement>
\<identifier> ::= \<cal> | \<joul> | \<rcp> | \<ing> | \<bool> | \<bowl> | \<function_decl> | \<unit>

## Control structures

\<let statement> ::= `let` \<rcp> `cook` \<expr> `in` \<expr>

- static semantics
  - evaluate e to a value v
  - v:t $\iff$ n:t
- dynamic semantics
  - evaluate e1 to a value v1
  - bind identifier n to value v1 in e2
  - evaluate e2 to a value v2
  - the value of the let_expr is v2

\<ternary statement> ::= `if` \<bool> `then` \<expr> `else` \<expr>

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

\<if statement> ::= `if` \<bool> `then` \<expr>

- static semantics
  - `p` evaluates to either true or false
  - `e1` evaluates to unit
- dynamic semantics
  - evaluate p to a value v
  - if v= true, then evaluate e to a value unit
  - if p then e1 evaluates to unit

## Declarations

\<function_decl> ::= `curry` \<rcp> `cook` \<expr> \
\<module_decl> ::= `shelf` \<rcp> `build` \<decl>\* `end`
