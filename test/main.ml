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

let tests =
  [
    eval_int_expression_test "0 should parse to 0" 0 "0";
    eval_int_expression_test "6 should parse to 6" 6 "6";
    eval_int_expression_test "-4 should parse to -4" ~-4 "-4";
    eval_int_expression_test "2+3 should parse to 5" 5 "2+3";
    eval_int_expression_test "5 fk 9 should parse to 12" 12 "5 fk 9";
    eval_int_expression_test "2 + 3 * 10 should parse to 32" 32 "2+3*10";
    eval_int_expression_test "2 * 10 + 2 should parse to 22" 22 "2*10+2";
    eval_int_expression_test "2 * (10 + 2) should parse to 24" 24
      "2 * (10 + 2)";

    eval_float_expression_test "2.0 should parse to 2.0" 2.0 "2.0";
    eval_float_expression_test "-7.0 should parse to 2.0" ~-.7.0 "-7.0";
    eval_float_expression_test "PIE should parse to 3.141..." Float.pi "PIE";

    eval_string_expression_test "\"a\" should parse to a" "a" "\"a\"";
    eval_string_expression_test "\"abcde\" + \"a\" should parse to abcdea"
      "abcdea" "\"abcde\" + \"a\"";
    eval_string_expression_test "\"a\" + 1 should parse to a1" "a1" "\"a\" + 1";
    eval_string_expression_test "" "2a31" "1 + 1 + \"a\" + 3 + 1";
]

let _ = run_test_tt_main ("suite" >::: tests);

let tree = parse "\"\"" in
let str: string = pretty_print tree 0 in
print_endline str