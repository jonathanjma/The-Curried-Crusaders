open Ast
(** The module [main.ml] contains functions for parsing and evaluating
    expressions in UStove. Much of the logic is handled by mutually recursive
    functions under the parent name [big_step], which steps an expression to a
    stable, value form. This module also includes [side_effects], which stores
    the output created by the program throughout evaluation. *)

open Env
open Print

exception Error of string
(** Represents an error thrown by the lexer or the parser. *)

(** Stores values to be printed as side effects. *)
let side_effects = ref ""

(** Given a lexer state, prints the current line and position of the lexer.
    Useful for printing lexer and parser errors. *)
let print_error_position lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.sprintf "Line:%d Position:%d" pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

(** [parse s] converts an input string to an expression represented by the AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  try
    let ast = Parser.top_expr Lexer.read lexbuf in
    ast
  with
  | Lexer.SyntaxError msg ->
      raise (Error (Printf.sprintf "%s: %s" (print_error_position lexbuf) msg))
  | Parser.Error ->
      raise
        (Error (Printf.sprintf "%s: syntax error" (print_error_position lexbuf)))

(** [parse_file] converts the contents of file with a list of definitions. *)
let parse_file (file_contents : string) : defn list =
  let lexbuf = Lexing.from_string file_contents in
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
  | Cal _ | Joul _ | Rcp _ | Bool _ | Bowl _ | FunctionClosure _ | Unit -> true
  | Binop _
  | Ternary _
  | Unop _
  | LetExpression _
  | Identifier _
  | Function _
  | FunctionApp _
  | LetDefinition (_, _) -> false

(** [big_step e] takes some expression [e] and computes a step of evaluation of
    [e] *)
let rec big_step (expression, env) : expr * Env.t =
  match expression with
  | Cal _ | Joul _ | Rcp _ | Bool _ | Unit | FunctionClosure _ | Bowl _ ->
      (expression, env)
  | Function (p, f) ->
      (FunctionClosure (Env.to_expr_list env, Function (p, f)), env)
  | Unop (op, e1) -> big_step (step_unop op e1 env, env)
  | Binop (bop, e1, e2) ->
      if bop = And || bop = Or then step_bool_op bop e1 e2 env
      else
        let v1, _ = big_step (e1, env) in
        let v2, _ = big_step (e2, env) in
        big_step (step_binop bop v1 v2, env)
  | Ternary (b1, e1, e2) -> big_step (step_ternary b1 e1 e2 env, env)
  | LetExpression (name, e1, e2) ->
      let v1, _ = big_step (e1, env) in
      let new_env : Env.t = Env.add_binding name v1 env in
      big_step (e2, new_env)
  | Identifier name -> big_step (step_identifier name env, env)
  | FunctionApp (f, e2) -> step_funcapp (fst (big_step (f, env))) e2 env
  | LetDefinition (n, e) ->
      failwith "a let definition must be a top level statement"

(** [step_funcapp f e2 env] steps a function application from the AST. Requires:
    [f] is a function closure and [e2] is a value. *)
and step_funcapp f e2 env =
  let v2 = big_step (e2, env) in
  match f with
  | FunctionClosure (env', Function (p, f')) ->
      big_step (LetExpression (p, fst v2, f'), Env.to_env env')
  | Identifier i -> (
      match Env.get_binding i env with
      | Some sv -> (
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
  | Cons, e1, e2 -> step_cons (e1, e2)
  | _ -> failwith "step_binop unimplemented (not a binary operator?)"

(** step_cons steps the Cons operator. Evaluate the first expression, then the
    second expression. Evaluates [e1 :: e2]. Requires: [e2] is of type Bowl *)
and step_cons (v1, v2) =
  match v2 with
  | Bowl [] -> Bowl [ v1 ]
  | Bowl lstItem -> Bowl (v1 :: lstItem)
  | _ -> failwith "step_cons Precondition violated"

(** [step_bool_op] steps a bool operation to an expression and an environment.
    Short circuit evaluation is implemented. *)
and step_bool_op bop e1 e2 env =
  let v1, env' = big_step (e1, env) in
  match (bop, v1) with
  | And, Bool b ->
      if not b then (Bool false, env')
      else
        let b2, env'' =
          match big_step (e2, env') with
          | Bool b2, env'' -> (b2, env'')
          | _ -> failwith "Type error"
        in
        if b2 then (Bool b2, env'') else (Bool false, env'')
  | Or, Bool b ->
      if b then (Bool true, env')
      else
        let b2, env'' =
          match big_step (e2, env') with
          | Bool b2, env'' -> (b2, env'')
          | _ -> failwith "Type error"
        in
        if b2 then (Bool b2, env'') else (Bool false, env'')
  | _ -> failwith "step_binop unimplemented (not a binary operator?)"

(** [step_identifier] steps an identifier based on its name and the current
    environment. If the environment find a binding associated with the
    identifier name, it returns the expression that the name is bound to.
    Raises: Failure, if the identifier is unbound. *)
and step_identifier name env =
  match Env.get_binding name env with
  | None -> failwith ("unbound identifier: " ^ name)
  | Some v -> v

(** [handleIntAndFloatOp] takes two expression, and the corresponding int and
    float operators for the binary operations, and returns an int or float
    expression, depending on the first two expressions. If either expression is
    a float, or both, return a [Joul] with [floatOp] applied to both [e1] and
    [e2] as floats. Otherwise, it returns a [Cal] with [intOp] applied to both
    [e1] and [e2] Raises: [Failure] if it is called and either [e1] or [e2] are
    not [Joul] or [Cal]*)
and handleIntAndFloatOp (e1, e2) intOp floatOp =
  match (e1, e2) with
  | Cal a, Cal b -> Cal (intOp a b)
  | Cal a, Joul b -> Joul (floatOp (float_of_int a) b)
  | Joul a, Cal b -> Joul (floatOp a (float_of_int b))
  | Joul a, Joul b -> Joul (floatOp a b)
  | _ -> failwith "Precondition violated"

(** [handleAdd] handles the addition of two expressions. It will return an
    expression that represents the addition of [e1] and [e2] *)
and handleAdd (e1, e2) =
  match (e1, e2) with
  | Rcp a, Rcp b -> Rcp (a ^ b)
  | Rcp a, Cal b -> Rcp (a ^ string_of_int b)
  | Cal a, Rcp b -> Rcp (string_of_int a ^ b)
  | Rcp a, Joul b -> Rcp (a ^ string_of_float b)
  | Joul a, Rcp b -> Rcp (string_of_float a ^ b)
  | Rcp a, Bool b -> Rcp (a ^ string_of_bool b)
  | Bool a, Rcp b -> Rcp (string_of_bool a ^ b)
  | _ -> handleIntAndFloatOp (e1, e2) ( + ) ( +. )

(** [handleComparison] takes two expressions and a comparison operator,
    evaluates the expressions to integer/float values, and returns a boolean
    value corresponding to the result of the comparison operation. *)
and handleComparison (e1, e2) (compOp : int -> int -> bool) : expr =
  match (e1, e2) with
  | Cal e1, Cal e2 -> Bool (compOp (Stdlib.compare e1 e2) 0)
  | Joul e1, Joul e2 -> Bool (compOp (Stdlib.compare e1 e2) 0)
  | Cal e1, Joul e2 -> Bool (compOp (Stdlib.compare (float_of_int e1) e2) 0)
  | Joul e1, Cal e2 -> Bool (compOp (Stdlib.compare e1 (float_of_int e2)) 0)
  | Rcp e1, Rcp e2 -> Bool (compOp (Stdlib.compare e1 e2) 0)
  | _, _ -> failwith "Type error: comparison doesn't apply to given types."

(** [step_ternary b1 e1 e2] steps a ternary expression, such that if [b1] is
    true, the expression evaluates to [step e1], and [step e2] if [b1] is false.
    If [b1] is not a boolean type, then the expression fails. *)
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

(** [step_unop] takes a some unary operation [op] of type [unop], and an
    expression of type [expr], as well as the current environment [env] of type
    [Env.t], and evaluates the unary operator applied to the expression in that
    environment, returning the evaluated expression. *)
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

(** [evalPrint] will take some an expression [expr] and environment [Env.t] pair
    [(e1, env)], and it will first evaluate [e1] in [env] to a value [v1], and
    then add the string evaluation of [v1] to [side_effects]. These
    [side_effects] integrated into UStove to be printed. *)
and evalPrint (e1, env) extra =
  let v1 = fst (big_step (e1, env)) in
  side_effects :=
    !side_effects
    ^ (if is_value v1 then Ast.string_of_val v1
      else failwith "Cannot print non-string type.")
    ^ extra;
  Unit

(** [global_env] is a [Env.t ref] that represents the global environment during
    interpretation *)
let global_env : Env.t ref = ref Env.empty

(** [eval e] evaluates [e] to some value [v]. *)
let rec eval (env : Env.t) (e : expr) : expr =
  if is_value e then e
  else
    let expr_after_step, env_after_step = big_step (e, env) in
    eval env_after_step expr_after_step

(** [get_side_effects ()] returns the side effects that have been accumulated
    throughout the evaluation for printing. As it does this, it then resets the
    [side_effects], so the next expression doesn't output the previous printing *)
let get_side_effects () : string =
  let my_side_effects = !side_effects in
  side_effects := "";
  my_side_effects

(** [interp s] is the string that results from the lexing, parsing, and
    evaluation of [s] *)
let interp (s : string) : string =
  s |> parse |> function
  | LetDefinition (n, e) ->
      let v, _ = big_step (e, !global_env) in

      let () = add_binding_mut n v global_env in

      "val " ^ n ^ " = " ^ Ast.string_of_val v
  | x -> x |> eval !global_env |> Ast.string_of_val

(** if [e] is a [LetDefinition], with identifier [n] and expression [e1], then
    [eval_wrapper e] evaluates to [Unit] and has the following side effects:

    - [e1] is evaluated to a value [v1]
    - a binding is added to the global environment [global_env], with identifier
      [n] being bound to [v1]
    - the evaluation of [e1] to [v1] may have side effects

    otherwise, if [e] is not a [LetDefinition], [eval_wrapper e] is equivalent
    to [eval !global_env e] *)
let eval_wrapper (e : expr) : expr =
  let return_value =
    match e with
    | LetDefinition (n, e1) ->
        let v, _ = big_step (e1, !global_env) in

        let () = add_binding_mut n v global_env in

        Unit
    | _ -> eval !global_env e
  in
  return_value

(** Evaluates a let definition in the context of a file. *)
let eval_wrapper_file (e : defn) =
  match e with
  | LetDef (n, e1) ->
      let v, _ = big_step (e1, !global_env) in

      let () = add_binding_mut n v global_env in

      ()

(** [string_of_val a] is a string representing the expr expression [a] *)
let string_of_val = Ast.string_of_val
