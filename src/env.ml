open Ast

type binding_value =
  | StandardValue of expr
  | FunctionClosureValue of t * string * expr

(* a closure represents the state of the environment at a certain time in the
   program's execution*)
and t = (string * binding_value) list

let make_standard_binding_value (e : expr) = StandardValue e
let empty = [] (* the empty environment *)

let rec remove_binding (binding_name : string) (env : t) : t =
  List.filter (fun (n, _) -> n <> binding_name) env

let add_binding (binding_name : string) (binding_value : binding_value)
    (env : t) =
  (* let lst = remove_binding binding_name env in (binding_name, binding_value)
     :: lst *)
  (binding_name, binding_value) :: env

let rec to_string_h : t -> string = function
  | [] -> ""
  | (b, _) :: remainder -> "( " ^ b ^ " )" ^ to_string_h remainder

let to_string : t -> string = fun (env : t) -> "[" ^ to_string_h env ^ "]"

let rec get_binding (binding_name : string) (env : t) : binding_value option =
  match env with
  | [] -> None
  | (name, value) :: remaining_bindings ->
      if name = binding_name then
        let () =
          match value with
          | StandardValue (Cal n) -> print_endline ""
          | _ -> failwith "aa"
        in

        Some value
      else get_binding binding_name remaining_bindings
