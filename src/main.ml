open Ast
open Env
open Print

exception Error of string

let side_effects = ref ""

let print_error_position lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.sprintf "Line:%d Position:%d" pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

(** [parse s] converts an input string to an expression. **)
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

(** [is_value e] returns whether or not [e] is a value. *)
let is_value (e : expr) : bool =
  match e with
  | Cal _
  | Joul _
  | Rcp _
  | Bool _
  | Bowl _
  | FunctionClosure _
  | Unit
  | Nil
  | Ing _ -> true
  | Binop _
  | Ternary _
  | Unop _
  | LetExpression _
  | Identifier _
  | Function _
  | FunctionApp _
  | LetDefinition (_, _) -> false

(** [step e] takes some expression [e] and computes a step of evaluation of [e] *)
let rec big_step (expression, env) : expr * Env.t =
  match expression with
  | Cal _ | Joul _ | Rcp _ | Bool _ | Unit | FunctionClosure _ ->
      (expression, env)
  | Function (p, f) ->
      (FunctionClosure (Env.to_expr_list env, Function (p, f)), env)
  | Unop (op, e1) -> big_step (step_unop op e1 env, env)
  | Binop (bop, e1, e2) ->
      let v1, _ = big_step (e1, env) in
      let v2, _ = big_step (e2, env) in
      big_step (step_binop bop v1 v2, env)
  | Ternary (b1, e1, e2) -> big_step (step_ternary b1 e1 e2 env, env)
  | LetExpression (name, e1, e2) ->
      let v1, _ = big_step (e1, env) in
      let new_env : Env.t =
        Env.add_binding name (Env.make_standard_binding_value v1) env
      in
      big_step (e2, new_env)
  | Identifier name -> big_step (step_identifier name env, env)
  | FunctionApp (f, e2) -> step_funcapp (fst (big_step (f, env))) e2 env
  | LetDefinition (n, e) ->
      failwith "a let definition must be a top level statement"
  | _ -> failwith "unmatched big_step"

(** step_funcapp steps a function application from the AST. *)
and step_funcapp f e2 env =
  let v2 = big_step (e2, env) in
  match f with
  | FunctionClosure (env', Function (p, f')) ->
      big_step (LetExpression (p, fst v2, f'), Env.to_env env')
  | Identifier i -> (
      match Env.get_binding i env with
      | Some (StandardValue sv) -> (
          match sv with
          | FunctionClosure (env', Function (p, f')) ->
              big_step (LetExpression (p, e2, f'), Env.to_env env')
          | _ -> failwith ("Type error: cannot apply " ^ i ^ " as a function"))
      | None -> failwith ("Unbound identifier " ^ i))
  | _ -> failwith "Type error: first expression is not a function closure."

(** [step_binop bop e1 e2] steps a binary operator that contains an operator and
    two values. Requires: [e1] and [e2] are values. *)
and step_binop bop e1 e2 =
  match (bop, e1, e2) with
  | Mult, e1, e2 -> handleIntAndFloatOp (e1, e2) ( * ) ( *. )
  | Fork, Cal a, Cal b -> Cal (Int.logxor a b)
  | Subtract, e1, e2 -> handleIntAndFloatOp (e1, e2) ( - ) ( -. )
  | Divide, e1, e2 -> handleIntAndFloatOp (e1, e2) ( / ) ( /. )
  | Add, e1, e2 -> handleAdd (e1, e2)
  | Mod, e1, e2 -> handleIntAndFloatOp (e1, e2) ( mod ) mod_float
  | Equal, e1, e2 -> handleComparison (e1, e2) ( = )
  | Less, e1, e2 -> handleComparison (e1, e2) ( < )
  | Greater, e1, e2 -> handleComparison (e1, e2) ( > )
  | Leq, e1, e2 -> handleComparison (e1, e2) ( <= )
  | Geq, e1, e2 -> handleComparison (e1, e2) ( >= )
  | _ -> failwith "step_binop unimplemented (not a binary operator?)"

and step_identifier name env =
  match Env.get_binding name env with
  | None -> failwith ("unbound identifier: " ^ name)
  | Some (StandardValue v) -> v

and handleIntAndFloatOp (e1, e2) intOp floatOp =
  match (e1, e2) with
  | Cal a, Cal b -> Cal (intOp a b)
  | Cal a, Joul b -> Joul (floatOp (float_of_int a) b)
  | Joul a, Cal b -> Joul (floatOp a (float_of_int b))
  | Joul a, Joul b -> Joul (floatOp a b)
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

and handleComparison (e1, e2) (compOp : int -> int -> bool) : expr =
  match (e1, e2) with
  | Cal e1, Cal e2 -> Bool (compOp (Stdlib.compare e1 e2) 0)
  | Joul e1, Joul e2 -> Bool (compOp (Stdlib.compare e1 e2) 0)
  | Cal e1, Joul e2 -> Bool (compOp (Stdlib.compare (float_of_int e1) e2) 0)
  | Joul e1, Cal e2 -> Bool (compOp (Stdlib.compare e1 (float_of_int e2)) 0)
  | Rcp e1, Rcp e2 -> Bool (compOp (Stdlib.compare e1 e2) 0)
  | Ing e1, Ing e2 -> Bool (compOp (Stdlib.compare e1 e2) 0)
  | _, _ -> failwith "Type error: comparison doesn't apply to given types."

(* [step_ternary b1 e1 e2] steps a ternary expression, such that if [b1] is
   true, the expression evaluates to [step e1], and [step e2] if [b1] is false.
   If [b1] is not a boolean type, then the expression fails.*)
and step_ternary b1 e1 e2 (env : Env.t) =
  match b1 with
  | Bool b ->
      if b then if is_value e1 then e1 else fst (big_step (e1, env))
      else if is_value e2 then e2
      else fst (big_step (e2, env))
  | b when is_value b ->
      (* b is a non-bolean value *)
      failwith
        "Type error: ternary expression must have boolean condition type."
  | _ -> step_ternary (fst (big_step (b1, env))) e1 e2 env

and step_unop op e1 (env : Env.t) =
  match op with
  | Unegation ->
      if is_value e1 then
        match e1 with
        | Cal a -> Cal ~-a
        | Joul b -> Joul ~-.b
        | _ -> failwith "Negative operator not applied to number"
      else Unop (Unegation, fst (big_step (e1, env)))
  | Boolnegation ->
      if is_value e1 then
        match e1 with
        | Bool b -> Bool (not b)
        | _ -> failwith "Boolnegation not applied to boolean"
      else Unop (Boolnegation, fst (big_step (e1, env)))
  | Print -> evalPrint (e1, env) ""
  | Println -> evalPrint (e1, env) "\n"

and evalPrint (e1, env) extra =
  let v1 = fst (big_step (e1, env)) in
  match v1 with
  | Rcp r ->
      side_effects := !side_effects ^ r ^ extra;
      Unit
  | Ing i ->
      side_effects := !side_effects ^ i ^ extra;
      Unit
  | _ -> failwith "Cannot print non-string type."

let global_env : Env.t ref = ref Env.empty

(** [eval e] evaluates [e] to some value [v]. *)
let rec eval (env : Env.t) (e : expr) : expr =
  if is_value e then e
  else
    let expr_after_step, env_after_step = big_step (e, env) in
    eval env_after_step expr_after_step

let eval_wrapper (e : expr) : expr = eval Env.empty e

let make_side_effects (e : expr) =
  eval_wrapper e;
  ()

let get_side_effects () : string =
  let my_side_effects = !side_effects in
  side_effects := "";
  my_side_effects

let interp (s : string) : string =
  s |> parse |> function
  | LetDefinition (n, e) ->
      let v, _ = big_step (e, !global_env) in

      let () = add_binding_m n (Env.make_standard_binding_value v) global_env in

      "val " ^ n ^ " = " ^ Ast.string_of_val v
  | x -> x |> eval !global_env |> Ast.string_of_val

let eval_wrapper (e : expr) : expr =
  let return_value =
    match e with
    | LetDefinition (n, e1) ->
        let v, _ = big_step (e1, !global_env) in

        let () =
          add_binding_m n (Env.make_standard_binding_value v) global_env
        in

        Unit
    | _ -> eval !global_env e
  in

  ( !global_env |> Env.to_string |> fun s ->
    print_endline "-------ENV-------";
    print_endline s;
    print_endline "-------EVAL-------" );

  return_value

let string_of_val = Ast.string_of_val
