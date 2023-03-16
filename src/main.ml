open Ast

let parse (s: string): expr = 
  let lexbuf = Lexing.from_string s in 
  let ast = Parser.prog Lexer.read lexbuf in 
  ast

(** [string_of_val e] converts [e] to a string.contents
    Requires: [e] is a value
    *)
let string_of_val (e: expr): string = 
  match e with 
  | Cal c -> string_of_int c
  | Joul j -> string_of_float j
  | Rcp s -> s
  | Binop _ -> failwith "Precondition violated"
  | _ -> failwith "Unimplemented"

(** [is_value e] returns whether or not [e] is a value. *)
let is_value (e: expr): bool = match e with
  | Cal _ -> true
  | Joul _ -> true
  | Rcp _ -> true
  | Binop _ -> false
  | _ -> failwith "Unimplemented"


  (** [step e] takes some expression e and computes a step of evaluation of [e] *)
let rec step: expr -> expr = function 
  | Cal _ -> failwith "Doesn't step"
  | Joul _ -> failwith "Doesn't step"
  | Rcp _ -> failwith "Doesn't step"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 ->
     step_binop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 -> Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) -> Binop (bop, step e1, e2) 
  | _ -> failwith "Unimplemented"
(* [step_binop bop e1 e2] steps a binary operator that contains 
    an operator and two values.
    Requires: [e1] and [e2] are values. *)
and step_binop bop e1 e2 = match (bop, e1, e2) with
| Add, Cal a, Cal b -> Cal (a + b)
| Mult, Cal a, Cal b -> Cal (a * b)
| Fork, Cal a, Cal b -> Cal (Int.logxor a b)
| Add, Rcp a, Rcp b -> Rcp (a ^ b)
| Add, Rcp a, Cal b -> Rcp (a ^ (string_of_int b))
| Add, Cal a, Rcp b -> Rcp ((string_of_int a) ^ b)
| _ -> failwith "Precondition violated"
 

(** [eval e] evaluates [e] to some value [v].  *)
let rec eval (e: expr): expr = 
  if is_value e then e else e |> step |> eval

let interp (s: string): string = 
  s |> parse |> eval |> string_of_val