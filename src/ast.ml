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
  | Unit
  | Unop of unop * expr
  | Bool of bool
  | Bowl of expr
  | Nil
  | Binop of bop * expr * expr
  | LetExpression of string * expr * expr
  | LetDefinition of string * expr
  | Function of string * expr
  | FunctionClosure of (string * expr) list * expr
  | Identifier of string
  | FunctionApp of expr * expr
  | Ternary of expr * expr * expr

let string_of_bop = function
  | Add -> "+"
  | Mult -> "*"
  | Fork -> "fk"
  | Subtract -> "-"
  | Divide -> "/"
  | Cons -> "::"
  | Geq -> ">="
  | Leq -> "<="
  | Equal -> "="
  | Greater -> ">"
  | Less -> "<"
  | Mod -> "mod"

let rec string_of_val (e : expr) : string =
  match e with
  | Cal c -> string_of_int c
  | Joul j -> string_of_float j
  | Rcp s -> "\"" ^ s ^ "\""
  | Bool b -> string_of_bool b
  | Unit -> "()"
  | Bowl b -> (
      match b with
      | Nil -> "[]"
      | _ -> "[" ^ string_of_bowl b ^ "]")
  | Nil -> "[]"
  | Binop (binop, e1, e2) -> string_of_val e1 ^ ""
  | Function (p, e) | FunctionClosure (_, Function (p, e)) ->
      "Function: f(" ^ p ^ ") = " ^ string_of_val e
  | Identifier s -> s
  | _ -> failwith "string_of_val unimplemented"

and string_of_bowl b =
  let rec string_of_bowl_tr acc = function
    | Nil -> acc
    | Binop (_, h, t) ->
        if t = Nil then acc ^ string_of_val h
        else string_of_bowl_tr (acc ^ string_of_val h ^ ", ") t
    | _ -> failwith "Precondition violated"
  in
  string_of_bowl_tr "" b
