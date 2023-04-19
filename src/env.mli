type t (* t represents an environment *)
type expression (* represents an expression *)
type value (* represents a value *)

val eval : t -> expression -> value
