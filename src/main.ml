open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [string_of_val e] converts [e] to a string.contents Requires: [e] is a value *)
let string_of_val (e : expr) : string =
  match e with
  | Cal c -> string_of_int c
  | Joul j -> string_of_float j
  | Rcp s -> s
  | Binop _ -> failwith "Precondition violated"
  | _ -> failwith "Unimplemented"

(** [is_value e] returns whether or not [e] is a value. *)
let is_value (e : expr) : bool =
  match e with
  | Cal _ -> true
  | Joul _ -> true
  | Rcp _ -> true
  | Binop _ -> false
  | _ -> failwith "Unimplemented"

(** [step e] takes some expression e and computes a step of evaluation of [e] *)
let rec step : expr -> expr = function
  | Cal _ -> failwith "Doesn't step"
  | Joul _ -> failwith "Doesn't step"
  | Rcp _ -> failwith "Doesn't step"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 -> step_binop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 -> Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) -> Binop (bop, step e1, e2)
  | _ -> failwith "Unimplemented"

(* [step_binop bop e1 e2] steps a binary operator that contains an operator and
   two values. Requires: [e1] and [e2] are values. *)
and step_binop bop e1 e2 =
  match (bop, e1, e2) with
  | Add, Cal a, Cal b -> Cal (a + b)
  | Mult, Cal a, Cal b -> Cal (a * b)
  | Fork, Cal a, Cal b -> Cal (Int.logxor a b)
  | Add, Rcp a, Rcp b -> Rcp (a ^ b)
  | Add, Rcp a, Cal b -> Rcp (a ^ string_of_int b)
  | Add, Cal a, Rcp b -> Rcp (string_of_int a ^ b)
  | _ -> failwith "Precondition violated"

(** [eval e] evaluates [e] to some value [v]. *)
let rec eval (e : expr) : expr = if is_value e then e else e |> step |> eval

let interp (s : string) : string = s |> parse |> eval |> string_of_val
let nl_l (level : int) : string = "\n" ^ String.make level ' '



let pretty_print_value (label: string) (f: 'a -> string) (value: 'a):  string =
  let string_representation : string = f value in
  label ^ " (" ^ string_representation ^ ")"





let rec pretty_print (e : expr) (level : int) : string =
  (* first, print the indentations *)
  let indentations : string = String.make (level * 2) ' ' in

  let rest : string =
    match e with
    | Cal a -> pretty_print_value "Cal" string_of_int a
    | Rcp a -> pretty_print_value "Rcp" (fun x -> x) a
    | Joul a -> pretty_print_value "Joul" string_of_float a
    | Bool a -> pretty_print_value "Bool" string_of_bool a

    | Identifier a -> pretty_print_value "Id" (fun x -> x) a

    | Binop (bop, e1, e2) -> pretty_print_binop bop e1 e2 level
        
    | LetExpression (name, e1, e2) -> pretty_print_let name e1 e2 level
        

    | Function (n, e) -> pretty_print_function n e level
      

    | FunctionApp (e1, e2) -> pretty_print_function_app e1 e2 level

    | Ternary (p, e1, e2) -> pretty_print_ternary p e1 e2 level
      
    | _ -> failwith "unimplemented"
  in

  indentations ^ rest


and pretty_print_binop (bop: bop) (e1: expr) (e2: expr) (level: int) : string = 
  let bop_string : string = bop_to_string bop in
        let pp_e1 : string = pretty_print e1 (level + 1) in
        let pp_e2 : string = pretty_print e2 (level + 1) in
        "Binop ("
        ^ nl_l (level + 2)
        ^ bop_string ^ ",\n" ^ pp_e1 ^ ",\n" ^ pp_e2 ^ nl_l level ^ ")"


and pretty_print_let (name: string) (e1: expr) (e2: expr) (level : int) : string =
  let name_string : string = nl_l (level + 2) ^ name in
          let e1_string : string = pretty_print e1 (level + 1) in
          let e2_string : string = pretty_print e2 (level + 1) in
          let end_paren_string : string = nl_l level ^ ")" in

          "Let (" ^ name_string ^ ",\n" ^ e1_string ^ ",\n" ^ e2_string
          ^ end_paren_string


and pretty_print_function (n: string) (e: expr) (level: int) : string =
  let arg_string: string = nl_l (level + 1) ^ n in
        let body_string: string = pretty_print e (level + 1) in
        let end_paren_string : string = nl_l level ^ ")" in
        "Func (" ^ arg_string ^ ",\n" ^ body_string ^ "," ^ end_paren_string


and pretty_print_function_app (e1: expr) (e2: expr) (level: int) : string = 
  let e1_string : string = pretty_print e1 (level + 1) in
        let e2_string : string = pretty_print e2 (level + 1) in
        let end_paren_string : string = nl_l level ^ ")" in
        "FuncApp (\n" ^ e1_string ^ ",\n" ^ e2_string ^ end_paren_string

and pretty_print_ternary (p: expr) (e1: expr) (e2: expr) (level: int) = 
  let p_string : string = pretty_print p (level + 1) in
        let e1_string : string = pretty_print e1 (level + 1) in
        let e2_string : string = pretty_print e2 (level + 1) in
        let end_paren_string : string = nl_l level ^ ")" in
        "Ternary (\n" ^ p_string ^ ",\n" ^ e1_string ^ ",\n" ^ e2_string ^ "" ^ end_paren_string