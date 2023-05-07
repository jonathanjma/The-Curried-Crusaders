type t
(** The type of the environment *)

val empty : t
(** The empty environment *)

val remove_binding : string -> t -> t
(** [remove_binding n env] is environment [env] without any bindings of the name
    [n]. *)

val add_binding : string -> Ast.expr -> t -> t
(** [add_binding n v env] is the environment [env] with a binding of [v] to name
    [n]. *)

val add_binding_mut : string -> Ast.expr -> t ref -> unit
(** [add_binding_mut n v env_ref] is the environment [!env_ref] with a binding
    of [v] to name [n]. Mutable version of [add_binding]. *)

val to_string : t -> string
(** [to_string env] is the environment [env] represented as a string. *)

val get_binding : string -> t -> Ast.expr option
(** [get_binding n env] returns a Some [v] if n is bound to v in [env] or None
    if [n] is not bound in [env]. *)

val to_expr_list : t -> (string * Ast.expr) list
(** Converts an environment type to a list of tuples containing the identifier
    and the AST representation of the bound value. *)

val to_env : (string * Ast.expr) list -> t
(** Converts a list of tuples containing the identifier and the AST
    representation of the bound value to an environment type. *)
