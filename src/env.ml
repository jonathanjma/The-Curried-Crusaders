open Ast

(* AF: The environment [(i1, x1); ... (in, xn)] represents the environment with
   value [xn] bound to identifier [in]. RI: the identifier [in] can only be
   bound to one value *)
type t = (string * expr) list

let empty = []

let rec remove_binding (binding_name : string) (env : t) : t =
  List.filter (fun (n, _) -> n <> binding_name) env

let add_binding (binding_name : string) (binding_value : expr) (env : t) =
  let lst = remove_binding binding_name env in
  (binding_name, binding_value) :: lst

let add_binding_mut (binding_name : string) (binding_value : expr)
    (env_ref : t ref) =
  let new_env : t = add_binding binding_name binding_value !env_ref in
  env_ref := new_env

let rec to_string : t -> string = fun (env : t) -> "[" ^ to_string_h env ^ "]"

and to_string_h : t -> string = function
  | [] -> ""
  | (b, v) :: remainder ->
      let v_string : string = string_of_val v in

      let new_binding_string : string = "(" ^ b ^ ", " ^ v_string ^ ")" in

      new_binding_string ^ to_string_h remainder

let rec get_binding (binding_name : string) (env : t) : expr option =
  match env with
  | [] -> None
  | (name, value) :: remaining_bindings ->
      if name = binding_name then Some value
      else get_binding binding_name remaining_bindings

let rec to_expr_list (env' : t) =
  match env' with
  | [] -> []
  | (name, value) :: remaining_bindings ->
      (name, value) :: to_expr_list remaining_bindings

let rec to_env (lst : (string * expr) list) : t =
  match lst with
  | [] -> []
  | (name, expr) :: remaining_bindings ->
      (name, expr) :: to_env remaining_bindings
