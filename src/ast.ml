(** The type of binary operators. *)
type bop =
  | Add
  | Mult
  | Fork (* Fork is the xor operator (xor logic gate looks like fork) *)
  | Subtract
  | Divide
  | Cons
  | Greater
  | Less
  | Leq
  | Geq
  | Equal
  | Mod

(** The type of unary operators *)
type unop =
  | Unegation
  | Boolnegation  (** The type of the abstract syntax tree (AST). *)
(* Unegation represents unary negation *)

let bop_to_string : bop -> string = function
  | Add -> "ADD"
  | Mult -> "MULT"
  | Fork -> "FORK"
  | Divide -> "DIVIDE"
  | Subtract -> "SUBTRACT"
  | Cons -> "CONS"
  | Greater -> "GREATER"
  | Less -> "LESS"
  | Leq -> "LEQ"
  | Geq -> "GEQ"
  | Equal -> "EQUAL"
  | Mod -> "MOD"

let unop_to_string : unop -> string = function
  | Unegation -> "UNEGATION"
  | Boolnegation -> "BOOLNEGATION"

type expr =
  | Cal of int
  | Joul of float
  | Rcp of string
  | Ing of string
  | Unop of unop * expr
  | Bool of bool
  | Bowl of expr
  | Nil
  | Binop of bop * expr * expr
  | LetExpression of string * expr * expr
  | Function of string * expr
  | Identifier of string
  | FunctionApp of expr * expr
  | Ternary of expr * expr * expr
