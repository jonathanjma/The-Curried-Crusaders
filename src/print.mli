(** [print.ml] is a module that handles the printing of expressions in the form
    of iCook's abstract syntax tree. The functions in this module process each
    expression and convert them into a string representing their AST form. *)

val pretty_print : Ast.expr -> int -> string
(** [pretty_print e l] is a string that represents the expression [e] with
    indentation level [l]. *)
