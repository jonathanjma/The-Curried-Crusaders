open OUnit2
open Interp
open Main

let id x = x

let eval_int_expression_test n eo i =
  n >:: fun _ -> assert_equal (string_of_int eo) (interp i) ~printer:id

let eval_float_expression_test n eo f =
  n >:: fun _ -> assert_equal (string_of_float eo) (interp f) ~printer:id

let eval_string_expression_test name expected_output string_expression =
  name >:: fun _ ->
  assert_equal expected_output (interp string_expression) ~printer:id

let parse_test (name : string) (input : string) (expected_output : Ast.expr) =
  name >:: fun _ -> assert_equal (parse input) expected_output

let eval_int_tests =
  [
    eval_int_expression_test "0 should parse to 0" 0 "0";
    eval_int_expression_test "6 should parse to 6" 6 "6";
    eval_int_expression_test "-4 should parse to -4" ~-4 "-4";
    eval_int_expression_test "2+3 should parse to 5" 5 "2+3";
    eval_int_expression_test "5 fk 9 should parse to 12" 12 "5 fk 9";
    eval_int_expression_test "2 + 3 * 10 should parse to 32" 32 "2+3*10";
    eval_int_expression_test "2 * 10 + 2 should parse to 22" 22 "2*10+2";
    eval_int_expression_test "2 * (10 + 2) should parse to 24" 24 "2 * (10 + 2)";
  ]

let eval_float_tests =
  [
    eval_float_expression_test "2.0 should parse to 2.0" 2.0 "2.0";
    eval_float_expression_test "-7.0 should parse to 2.0" ~-.7.0 "-7.0";
    eval_float_expression_test "PIE should parse to 3.141..." Float.pi "PIE";
  ]

let eval_string_tests =
  [
    eval_string_expression_test "\"a\" should parse to a" "a" "\"a\"";
    eval_string_expression_test "\"abcde\" + \"a\" should parse to abcdea"
      "abcdea" "\"abcde\" + \"a\"";
    eval_string_expression_test "\"a\" + 1 should parse to a1" "a1" "\"a\" + 1";
    eval_string_expression_test "" "2a31" "1 + 1 + \"a\" + 3 + 1";
  ]

let eval_tests =
  List.flatten [ eval_int_tests; eval_float_tests; eval_string_tests ]

let parse_int_tests = [ 
  parse_test "parse 1" "1" (Cal 1);
  parse_test "parse 0" "0" (Cal 0);
  parse_test "parse 12345" "12345" (Cal 12345);
  parse_test "parse -1" "-1" (Cal (-1));
  parse_test "parse -99999" "-99999" (Cal (-99999));
]

let parse_bool_tests = [
  parse_test "parse true" "true" (Bool true);
  parse_test "parse false" "false" (Bool false);
]

let parse_string_tests = [
  parse_test "parse \"test\"" "\"test\"" (Rcp "test");
  parse_test "parse \"\"" "\"\"" (Rcp ""); (* FAILING *)
  parse_test "parse \"a a\"" "\"a a\"" (Rcp "a a"); (* FAILING *)
  parse_test "parse \"this is a test\"" "\"this is a test\"" (Rcp "this is a test"); (* FAILING *)
  parse_test "parse \" \"" "\" \"" (Rcp " ") (* FAILING *)
]


let parse_tests = List.flatten [ parse_int_tests; parse_bool_tests; ]
let tests = List.flatten [ eval_tests; parse_tests; ]
let () = run_test_tt_main ("suite" >::: tests)
