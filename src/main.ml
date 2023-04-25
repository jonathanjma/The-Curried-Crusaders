open Ast

exception Error of string

let print_error_position lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.sprintf "Line:%d Position:%d" pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  try
    let ast = Parser.prog Lexer.read lexbuf in
    ast
  with
  | Lexer.SyntaxError msg ->
      raise (Error (Printf.sprintf "%s: %s" (print_error_position lexbuf) msg))
  | Parser.Error ->
      raise
        (Error (Printf.sprintf "%s: syntax error" (print_error_position lexbuf)))

(** [string_of_val e] converts [e] to a string.contents Requires: [e] is a value *)
let rec string_of_val (e : expr) : string =
  match e with
  | Cal c -> string_of_int c
  | Joul j -> string_of_float j
  | Rcp s -> "\"" ^ s ^ "\""
  | Bool b -> string_of_bool b
  | Bowl b -> (
      match b with
      | Nil -> "[]"
      | _ -> "[" ^ string_of_bowl b ^ "]")
  | Nil -> "[]"
  | Binop _ -> failwith "string of val Precondition violated"
  | Function (p, e1) -> failwith "String of function unimplemented"
  | _ -> failwith "string of val Unimplemented"

and string_of_bowl b =
  let rec string_of_bowl_tr acc = function
    | Nil -> acc
    | Binop (_, h, t) ->
        if t = Nil then acc ^ string_of_val h
        else string_of_bowl_tr (acc ^ string_of_val h ^ ", ") t
    | _ -> failwith "Precondition violated"
  in
  string_of_bowl_tr "" b

(** [is_value e] returns whether or not [e] is a value. *)
let is_value (e : expr) : bool =
  match e with
  | Cal _
  | Joul _
  | Rcp _
  | LetExpression _
  | Bool _
  | Bowl _
  | Function _
  | Nil
  | Ing _ -> true
  | Binop _ | Ternary _ | Unop _ | FunctionApp _ | Identifier _ -> false

let to_binding_value (e : expr) =
  match e with
  | Cal c -> Env.CalValue c
  | Rcp r -> Env.RcpValue r
  | Joul j -> Env.JoulValue j
  | Ing i -> Env.IngValue i
  | Bool b -> Env.BoolValue b
  | _ -> failwith "Not a value or unimplemented to_binding_value"

let expression_of_binding_value = function
  | Env.CalValue c -> Cal c
  | Env.RcpValue r -> Rcp r
  | Env.JoulValue j -> Joul j
  | Env.IngValue i -> Ing i
  | Env.BoolValue b -> Bool b
  | _ -> failwith "Unimplemented: expression_of_binding_value"

(** [step env e] takes some expression [e] and computes a step of evaluation in
    environment [env] *)
let rec step (env : Env.t) (exp : expr) : expr =
  match exp with
  | Cal _ -> failwith "Doesn't step"
  | Joul _ -> failwith "Doesn't step"
  | Rcp _ -> failwith "Doesn't step"
  | Function _ -> failwith "Doesn't step"
  | FunctionApp (e1, e2) -> step_funcapp env e1 e2
  | Unop (op, e1) -> step_unop env op e1
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 ->
      step_binop env bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 -> Binop (bop, e1, step env e2)
  | Binop (bop, e1, e2) -> Binop (bop, step env e1, e2)
  | Ternary (b1, e1, e2) -> step_ternary env b1 e1 e2
  | Identifier i -> (
      match Env.get_binding i env with
      | Some bv -> expression_of_binding_value bv
      | None -> failwith ("Unbound identifier " ^ i))
  | _ -> failwith "Unimplemented"

(* [step_binop bop e1 e2] steps a binary operator that contains an operator and
   two values. Requires: [e1] and [e2] are values. *)
and step_binop env bop e1 e2 =
  match (bop, e1, e2) with
  | Mult, e1, e2 -> handleIntAndFloatOp (e1, e2) ( * ) ( *. )
  | Fork, Cal a, Cal b -> Cal (Int.logxor a b)
  | Subtract, e1, e2 -> handleIntAndFloatOp (e1, e2) ( - ) ( -. )
  | Divide, e1, e2 -> handleIntAndFloatOp (e1, e2) ( / ) ( /. )
  | Add, a, b -> handleAdd (a, b)
  | Greater, e1, e2 -> handleComparison (e1, e2) ( > )
  | Less, e1, e2 -> handleComparison (e1, e2) ( < )
  | Leq, e1, e2 -> handleComparison (e1, e2) ( <= )
  | Geq, e1, e2 -> handleComparison (e1, e2) ( >= )
  | Equal, e1, e2 -> handleComparison (e1, e2) ( = )
  | _ -> failwith "Type error: those types do not work the binary operator"

and handleIntAndFloatOp (e1, e2) intOp floatOp =
  match (e1, e2) with
  | Cal a, Cal b -> Cal (intOp a b)
  | Cal a, Joul b -> Joul (floatOp (float_of_int a) b)
  | Joul a, Cal b -> Joul (floatOp a (float_of_int b))
  | Joul a, Joul b -> Joul (floatOp a b)
  | _ -> failwith "Precondition violated"

and handleComparison (e1, e2) (compOp : 'a -> 'a -> bool) =
  match (e1, e2) with
  | Cal a, Cal b -> Bool (compOp (float_of_int a) (float_of_int b))
  | Cal a, Joul b -> Bool (compOp (float_of_int a) b)
  | Joul a, Cal b -> Bool (compOp a (float_of_int b))
  | Joul a, Joul b -> Bool (compOp a b)
  | _ -> failwith "Precondition violated"

and handleAdd (e1, e2) =
  match (e1, e2) with
  | Rcp a, Rcp b -> Rcp (a ^ b)
  | Rcp a, Cal b -> Rcp (a ^ string_of_int b)
  | Cal a, Rcp b -> Rcp (string_of_int a ^ b)
  | Rcp a, Joul b -> Rcp (a ^ string_of_float b)
  | Joul a, Rcp b -> Rcp (string_of_float a ^ b)
  | Rcp a, Bool b -> Rcp (a ^ string_of_bool b)
  | Bool a, Rcp b -> Rcp (string_of_bool a ^ b)
  | Rcp a, Ing b -> Rcp (a ^ b)
  | Ing a, Rcp b -> Rcp (a ^ b)
  | _ -> handleIntAndFloatOp (e1, e2) ( + ) ( +. )

(** First evaluate [e2] to a value [v1]. Then bind [param] to [v1] in [env],
    call that resulting environment [env']. Finally, evaluate [e1] in [env'] *)
and step_funcapp env e1 e2 =
  match e1 with
  (* e2 gets stepped to some value v1, then v1 is bind to [param] in env *)
  | Function (param, body) ->
      let v1 = if is_value e2 then e2 else step env e2 in
      let env' = Env.add_binding param (to_binding_value v1) env in
      step env' body
  | _ -> failwith "Type error"

(* [step_ternary b1 e1 e2] steps a ternary expression, such that if [b1] is
   true, the expression evaluates to [step e1], and [step e2] if [b1] is false.
   If [b1] is not a boolean type, then the expression fails.*)
and step_ternary (env : Env.t) b1 e1 e2 =
  match b1 with
  | Bool b ->
      if b then if is_value e1 then e1 else step env e1
      else if is_value e2 then e2
      else step env e2
  | b when is_value b ->
      (* b is a non-bolean value *)
      failwith
        "Type error: ternary expression must have boolean condition type."
  | _ -> step_ternary env (step env b1) e1 e2

and step_unop (env : Env.t) op e1 =
  match op with
  | Unegation ->
      if is_value e1 then
        match e1 with
        | Cal a -> Cal ~-a
        | Joul b -> Joul ~-.b
        | _ -> failwith "Type error"
      else Unop (Unegation, step env e1)

(** Find the binding for [id] in enviornment [env] *)
and step_identifier id env = failwith "Unimplemented step identifier."

(** [eval e] evaluates [e] to some value [v] under enviornment [env]. *)
let rec eval (env : Env.t) (e : expr) : expr =
  if is_value e then e else eval env (step env e)

let interp (s : string) : string = s |> parse |> eval Env.empty |> string_of_val
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
    | Nil -> pretty_print_value "Nil" (fun x -> x) "[]"
    | Identifier a -> pretty_print_value "Id" (fun x -> x) a
    | Bowl e -> pretty_print_bowl e level
    | Binop (bop, e1, e2) -> pretty_print_binop bop e1 e2 level
    | LetExpression (name, e1, e2) -> pretty_print_let name e1 e2 level
    | Function (n, e) -> pretty_print_function n e level
    | FunctionApp (e1, e2) -> pretty_print_function_app e1 e2 level
    | Ternary (p, e1, e2) -> pretty_print_ternary p e1 e2 level
    | Unop (op, e1) -> pretty_print_unop op e1 level
    (* | _ -> failwith "unimplemented" *)
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
