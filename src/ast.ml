(** The type of binary operators. *)
type bop =
  | Add
  | Mult
  | Fork (* Fork is the xor operator (xor logic gate looks like fork) *)

(** The type of the abstract syntax tree (AST). *)
type expr =
  | Cal of int
  | Joul of float
  | Rcp of string
  | Ing of char
  | Bool of bool
  | Bowl of expr list
  | Binop of bop * expr * expr
