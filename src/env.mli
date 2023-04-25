open Ast

type t

type binding_value =
  | CalValue of int
  | JoulValue of float
  | RcpValue of string
  | IngValue of string
  | BoolValue of bool
  | BowlValue of binding_value list
  | FunctionClosureValue of t * string * expr

val empty : t (* an environment with no bindings *)
val remove_binding : string -> t -> t
val add_binding : string -> binding_value -> t -> t
val get_binding : string -> t -> binding_value option
