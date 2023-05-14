open Test_util

let eval_int_tests =
  [
    eval_int_expression_test "0 should parse to 0" 0 "0";
    eval_int_expression_test "6 should parse to 6" 6 "6";
    eval_int_expression_test "~4 should parse to -4" ~-4 "~4";
    eval_int_expression_test "2+3 should parse to 5" 5 "2+3";
    eval_int_expression_test "5 fk 9 should parse to 12" 12 "5 fk 9";
    eval_int_expression_test "2 + 3 * 10 should parse to 32" 32 "2+3*10";
    eval_int_expression_test "2 * 10 + 2 should parse to 22" 22 "2*10+2";
    eval_int_expression_test "2 * (10 + 2) should parse to 24" 24 "2 * (10 + 2)";
    eval_int_expression_test "2 * ~10 / 5 should parse to -4" ~-4
      "2 * ~10 /\n       5 ";
    eval_int_expression_test "2 - 2 - 3 should parse to 1" ~-3
      "2 - 2 -\n       3";
    eval_int_expression_test "2 - ~2 - 2 should parse to 2" 2 "2 - ~2 - 2";
    eval_int_expression_test "10 / 2 / 5 should parse to 1" 1 "10 / 2 / 5";
    eval_int_expression_test "~(2 - ~3) should parse to -5" ~-5 "~(2 -  ~3)";
  ]

let eval_float_tests =
  [
    eval_float_expression_test "2.0 should parse to 2.0" 2.0 "2.0";
    eval_float_expression_test "-7.0 should parse to -7.0" (-7.0) "~7.0";
    eval_float_expression_test "PIE should parse to 3.141..." Float.pi "PIE";
  ]

let eval_string_tests =
  [
    eval_string_expression_test "\"a\" should parse to \"a\"" "\"a\"" "\"a\"";
    eval_string_expression_test "\"abcde\" + \"a\" should parse to abcdea"
      "\"abcdea\"" "\"abcde\" + \"a\"";
    eval_string_expression_test "\"a\" + 1 should parse to a1" "\"a1\""
      "\"a\" + 1";
    eval_autonamed_string_expression_test "\"2a31\"" "1 + 1 + \"a\" + 3 + 1";
    eval_autonamed_string_expression_test "\"2.a1.110\""
      "2.0 + \"a\" +  1.1  + \"1\" + 0";
    eval_autonamed_string_expression_test
      ("\"so true that PI(E) = " ^ string_of_float Float.pi ^ "\"")
      "\"so \" + true + \" that PI(E) = \" + PIE";
  ]

let eval_ternary_tests =
  [
    eval_expression_test "if true then 1 else 0 -> 1" "1"
      "if true then 1 else 0";
    eval_expression_test "if false then 1 else 0 -> 0" "0"
      "if false then 1 else 0";
    eval_expression_test "if true then 1    +  1 *  4 else 3 + 2 + 1 * 1 -> 5"
      "5" "if true then 1    +  1 *  4 else 3 + 2 + 1 * 1";
    eval_expression_test "" "5" "if true then 5 else 10";
    eval_expression_test "" "\"two is less than three\""
      "if 2 < 3 then \"two is less than three\" else \"two is not less than \
       three\"";
  ]

let eval_binop_tests =
  [
    eval_float_expression_test "2 + 3.0 -> 5.0" 5.0 "2 + 3.0";
    eval_float_expression_test "3.0 + 5.0 * 2 / 1.0 / 2 -> 8.0" 8.0
      "3.0 + 5.0 * 2 / 1.0 / 2";
  ]

let eval_function_tests =
  [
    eval_autonamed_string_expression_test "2" "(curry x cook x+1) 1";
    eval_autonamed_string_expression_test "-1"
      "(curry x cook (curry y cook y-x) 2) 3";
    eval_autonamed_string_expression_test "4"
      "(curry x cook (curry x cook x+x) 2) 1";
    eval_autonamed_string_expression_test "true" "(curry x cook (x % 2 = 0)) 10";
    eval_autonamed_string_expression_test "24"
      {|
      (curry x cook (curry y cook (curry z cook x*y*z) 2) 3) 4
    |};
    eval_autonamed_string_expression_test "24."
      {|
     (curry x cook (curry y cook (curry z cook x*y*z) 2.0) 3) 4.0 |};
    eval_autonamed_string_expression_test "42"
      {|
    let inc cook (curry x cook x+1) in (
      let double cook (curry x cook x*2) in (
        double (inc (inc 19))
      )
    )
      |};
  ]

let eval_let_expression_tests =
  [
    eval_expression_test "" "1" "let a cook 1 in a";
    eval_expression_test "" "2" "let a cook 1 in a + 1";
    eval_expression_test "" "1" "let a cook 1 in (1)";
    eval_expression_test "" "15"
      {|
  let a cook 1 in
  let b cook 2 in
  let c cook 3 in
  let d cook 4 in
  let e cook 5 in
  a + b + c + d + e
  |};
    eval_expression_test "" {|"e"|}
      {|
  let a cook "a" in
  let b cook "b" in
  let c cook "c" in
  let d cook "d" in
  let e cook "e" in
  
  if true then e else d
  |};
    eval_expression_test "" {|5|}
      {|
  let succ cook (curry n cook n + 1) in
  let a cook 1 in
  let b cook 2 in
  succ (succ (a + b))
  |};
  ]

let eval_bowl_tests =
  [
    eval_expression_test "nil" "[]" "[]";
    eval_expression_test "" "[5]" "5 :: []";
    eval_expression_test "" "[5, 6]" "5 :: [6]";
    eval_expression_test "" "[5, true]" "5 :: [true]";
    eval_expression_test "" "[5, [4], 6]" "5 :: [4] :: [6]";
    eval_expression_test "" "[4, true, [8]]" "[4, true, [8]]";
  ]

let eval_tests =
  List.flatten
    [
      eval_int_tests;
      eval_float_tests;
      eval_string_tests;
      eval_ternary_tests;
      eval_binop_tests;
      eval_function_tests;
      eval_let_expression_tests;
      eval_bowl_tests;
    ]
