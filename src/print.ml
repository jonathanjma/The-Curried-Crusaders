open Ast

let nl_l (level : int) : string = "\n" ^ String.make level ' '

let pretty_print_value (label : string) (f : 'a -> string) (value : 'a) : string
    =
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
    | Ing a -> pretty_print_value "Ing" (fun x -> x) a
    | Unit -> "Unit"
    | Nil -> pretty_print_value "Nil" (fun x -> x) "[]"
    | Identifier a -> pretty_print_value "Id" (fun x -> x) a
    | Bowl e -> pretty_print_bowl e level
    | Binop (bop, e1, e2) -> pretty_print_binop bop e1 e2 level
    | LetExpression (name, e1, e2) -> pretty_print_let name e1 e2 level
    | LetDefinition (name, e) -> pretty_print_let_definition name e level
    | Function (n, e) -> pretty_print_function n e level
    | FunctionApp (e1, e2) -> pretty_print_function_app e1 e2 level
    | Ternary (p, e1, e2) -> pretty_print_ternary p e1 e2 level
    | Unop (op, e1) -> pretty_print_unop op e1 level
    | _ -> failwith "unimplemented"
  in
  indentations ^ rest

and pretty_print_bowl (e : expr) (level : int) : string =
  let pp_e : string = pretty_print e (level + 1) in
  "Bowl (" ^ nl_l (level + 1) ^ pp_e ^ nl_l (level + 1) ^ ")"

and pretty_print_binop (bop : bop) (e1 : expr) (e2 : expr) (level : int) :
    string =
  let bop_string : string = bop_to_string bop in
  let pp_e1 : string = pretty_print e1 (level + 1) in
  let pp_e2 : string = pretty_print e2 (level + 1) in
  "Binop ("
  ^ nl_l (level + 2)
  ^ bop_string ^ ",\n" ^ pp_e1 ^ ",\n" ^ pp_e2
  ^ nl_l (level + 1)
  ^ ")"

and pretty_print_unop (op : unop) (e1 : expr) (level : int) : string =
  let op_string : string = unop_to_string op in
  let pp_e1 : string = pretty_print e1 (level + 1) in
  "Unop ("
  ^ nl_l (level + 2)
  ^ op_string ^ ",\n" ^ pp_e1
  ^ nl_l (level + 1)
  ^ ")"

and pretty_print_let (name : string) (e1 : expr) (e2 : expr) (level : int) :
    string =
  let name_string : string = nl_l (level + 2) ^ name in
  let e1_string : string = pretty_print e1 (level + 1) in
  let e2_string : string = pretty_print e2 (level + 1) in
  let end_paren_string : string = nl_l (level + 1) ^ ")" in

  "Let (" ^ name_string ^ ",\n" ^ e1_string ^ ",\n" ^ e2_string
  ^ end_paren_string

and pretty_print_let_definition (name : string) (e1 : expr) (level : int) :
    string =
  let name_string : string = nl_l (level + 2) ^ name in
  let e1_string : string = pretty_print e1 (level + 1) in
  let end_paren_string : string = nl_l (level + 1) ^ ")" in

  "Let (" ^ name_string ^ ",\n" ^ e1_string ^ end_paren_string

and pretty_print_function (n : string) (e : expr) (level : int) : string =
  let arg_string : string = nl_l (level + 2) ^ n in
  let body_string : string = pretty_print e (level + 1) in
  let end_paren_string : string = nl_l (level + 1) ^ ")" in
  "Func (" ^ arg_string ^ ",\n" ^ body_string ^ "," ^ end_paren_string

and pretty_print_function_app (e1 : expr) (e2 : expr) (level : int) : string =
  let e1_string : string = pretty_print e1 (level + 1) in
  let e2_string : string = pretty_print e2 (level + 1) in
  let end_paren_string : string = nl_l (level + 1) ^ ")" in
  "FuncApp (\n" ^ e1_string ^ ",\n" ^ e2_string ^ end_paren_string

and pretty_print_ternary (p : expr) (e1 : expr) (e2 : expr) (level : int) =
  let p_string : string = pretty_print p (level + 1) in
  let e1_string : string = pretty_print e1 (level + 1) in
  let e2_string : string = pretty_print e2 (level + 1) in
  let end_paren_string : string = nl_l (level + 1) ^ ")" in
  "Ternary (\n" ^ p_string ^ ",\n" ^ e1_string ^ ",\n" ^ e2_string ^ ""
  ^ end_paren_string
