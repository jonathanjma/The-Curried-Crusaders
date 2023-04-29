open Ast

type binding_value = StandardValue of expr

(* a closure represents the state of the environment at a certain time in the
   program's execution*)
and t = (string * binding_value) list

let make_standard_binding_value (e : expr) = StandardValue e
let empty = [] (* the empty environment *)

let rec remove_binding (binding_name : string) (env : t) : t =
  List.filter (fun (n, _) -> n <> binding_name) env

let add_binding (binding_name : string) (binding_value : binding_value)
    (env : t) =
  let lst = remove_binding binding_name env in
  (binding_name, binding_value) :: lst

let add_binding_m (binding_name : string) (binding_value : binding_value)
    (env_ref : t ref) =
  let new_env : t = add_binding binding_name binding_value !env_ref in
  env_ref := new_env

let rec to_string_h : t -> string = function
  | [] -> ""
  | (b, v) :: remainder ->
      let v_string : string =
        match v with
        | StandardValue v' -> string_of_val v'
      in

      let new_binding_string : string = "(" ^ b ^ ", " ^ v_string ^ ")" in

      new_binding_string ^ to_string_h remainder

let to_string : t -> string = fun (env : t) -> "[" ^ to_string_h env ^ "]"

let rec get_binding (binding_name : string) (env : t) : binding_value option =
  match env with
  | [] -> None
  | (name, value) :: remaining_bindings ->
      if name = binding_name then Some value
      else get_binding binding_name remaining_bindings

let rec to_expr_list (env' : t) =
  match env' with
  | [] -> []
  | (name, value) :: remaining_bindings -> (
      match value with
      | StandardValue v -> (name, v) :: to_expr_list remaining_bindings)

let rec to_env (lst : (string * expr) list) : t =
  match lst with
  | [] -> []
  | (name, expr) :: remaining_bindings ->
      (name, StandardValue expr) :: to_env remaining_bindings
