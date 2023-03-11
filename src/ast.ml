(** The type of binary operators. *)
type bop = 
  | Add
  | Mult
  | Fork (* Fork is the xor operator (xor logic gate looks like fork) *)
  
(** The type of the abstract syntax tree (AST). *)
type expr = 
  | Cal of int 
  | Binop of bop * expr * expr