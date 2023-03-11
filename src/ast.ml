(** The type of binary operators. *)

(** The type of the abstract syntax tree (AST). *)
type expr =
  | Cal of int
  | Joul of float
  | Rcp of string
  | Ing of char
  | Bool of bool
  | Bowl of expr list
