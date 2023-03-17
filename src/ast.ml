(** The type of binary operators. *)
type bop =
  | Add
  | Mult
  | Fork (* Fork is the xor operator (xor logic gate looks like fork) *)

(** The type of the abstract syntax tree (AST). *)

let bop_to_string : bop -> string = function
  | Add -> "ADD"
  | Mult -> "MULT"
  | Fork -> "FORK"

type expr =
  | Cal of int
  | Joul of float
  | Rcp of string
  | Ing of char
  | Bool of bool
  | Bowl of expr list
  | Binop of bop * expr * expr
  | LetExpression of string * expr * expr
  | Function of string * expr
  | Identifier of string
  | FunctionApp of expr * expr
  (* string is the parameter*)
  (* expr is the body of the function *)
