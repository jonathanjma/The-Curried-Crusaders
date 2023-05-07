open OUnit2
open Interp
open Main

let id x = x

let eval_int_expression_test n eo i =
  n >:: fun _ -> assert_equal (string_of_int eo) (interp i) ~printer:id

let eval_expression_test n eo i =
  n >:: fun _ -> assert_equal eo (interp i) ~printer:id

let eval_float_expression_test n eo f =
  n >:: fun _ -> assert_equal (string_of_float eo) (interp f) ~printer:id

let eval_string_expression_test name expected_output string_expression =
  name >:: fun _ ->
  assert_equal expected_output (interp string_expression) ~printer:id

let icook_str_of_int n1 =
  let n1_str = string_of_int n1 in
  if n1 >= 0 then n1_str
  else "~" ^ String.sub n1_str 1 (String.length n1_str - 1)

let read_file_test (name : string) (rel_dir : string) (expected_output : string)
    =
  name >:: fun _ ->
  let dir : string = "test/programs/" ^ rel_dir ^ ".icook" in
  assert_equal (Filereader.read dir) expected_output

(** This function creates a test with an automated name. Its first paremeter is
    the expected output and the second parameter is the string expression. The
    automaterd name of the test is "[string expression] should parse to
    [expected output]" *)
let eval_autonamed_string_expression_test expected_output string_expression =
  eval_string_expression_test
    (string_expression ^ " should evaluate to " ^ expected_output)
    expected_output string_expression

let env_equality_test expected_env string_expression =
  eval_string_expression_test
    (string_expression ^ " should produce the env " ^ Env.to_string expected_env)
    (Env.to_string expected_env)
    (Env.to_string !Main.global_env)

let parse_test (name : string) (input : string) (expected_output : Ast.expr) =
  name >:: fun _ -> assert_equal (parse input) expected_output

let icook_string_of_int i =
  if i < 0 then "~" ^ string_of_int ~-i else string_of_int i

let icook_string_of_float (i : float) =
  if i < 0. then "~" ^ string_of_float (i *. -1.) else string_of_float i