open OUnit2
open Interp
open Main

(* Random tests are performance intensive, you may not want to run them every
   time! *)
let run_random_tests = true

(* This number denotes how many of each type of random test to generate *)
let number_of_random_tests = 5000
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

let parse_int_tests =
  [
    parse_test "parse 1" "1" (Cal 1);
    parse_test "parse 0" "0" (Cal 0);
    parse_test "parse 12345" "12345" (Cal 12345);
    parse_test "parse -1" "-1" (Cal (-1));
    parse_test "parse -99999" "-99999" (Cal (-99999));
  ]

let parse_bool_tests =
  [
    parse_test "parse true" "true" (Bool true);
    parse_test "parse false" "false" (Bool false);
  ]

let parse_string_tests =
  [
    parse_test "parse \"test\"" "\"test\"" (Rcp "test");
    parse_test "parse \"\"" "\"\"" (Rcp "");
    (* FAILING *)
    parse_test "parse \"a a\"" "\"a a\"" (Rcp "a a");
    (* FAILING *)
    parse_test "parse \"this is a test\"" "\"this is a test\""
      (Rcp "this is a test");
    (* FAILING *)
    parse_test "parse \" \"" "\" \"" (Rcp " ") (* FAILING *);
  ]

let string_of_bop = function
  | Ast.Add -> "+"
  | Ast.Mult -> "*"
  | Ast.Fork -> "fk"

let random_parse_binop_tests (tests : int) =
  let rec random_parse_binop_tests (tests : int) (acc : test list) =
    if tests = 0 then acc
    else
      let bop : Ast.bop =
        match Random.int 3 with
        | 0 -> Add
        | 1 -> Mult
        | 2 -> Fork
        | _ -> failwith "this shouldn't run"
      in

      let a : string = Random.int 10000 |> string_of_int in
      let b : string = Random.int 10000 |> string_of_int in

      let new_test : test =
        parse_test "parse"
          (a ^ " " ^ string_of_bop bop ^ " " ^ b)
          (Binop (bop, Cal (int_of_string a), Cal (int_of_string b)))
      in
      random_parse_binop_tests (tests - 1) (new_test :: acc)
  in

  random_parse_binop_tests tests []

let random_parse_float_tests (tests : int) =
  let rec random_parse_float_tests (tests : int) (acc : test list) =
    let test_float : float ref =
      ref (Random.float 10000. |> string_of_float |> float_of_string)
    in

    let () = if Random.bool () then test_float := -1. *. !test_float in

    if tests = 0 then acc
    else
      let new_test : test =
        parse_test
          ("parse " ^ string_of_float !test_float ^ "0")
          (string_of_float !test_float ^ "0")
          (Joul !test_float)
      in
      random_parse_float_tests (tests - 1) (new_test :: acc)
  in
  random_parse_float_tests tests []

let random_parse_int_tests (tests : int) =
  let rec random_parse_int_tests (tests : int) (acc : test list) =
    let test_int : int ref = ref (Random.bits ()) in
    let () = if Random.bool () then test_int := -1 * !test_int in

    if tests = 0 then acc
    else
      let new_test : test =
        parse_test
          ("parse " ^ string_of_int !test_int)
          (string_of_int !test_int) (Cal !test_int)
      in
      random_parse_int_tests (tests - 1) (new_test :: acc)
  in
  random_parse_int_tests tests []

let random_parse_string_tests (tests : int) =
  let make_rnd_str (length : int) =
    let rec make_rnd_str (length : int) (acc : string) =
      let rand_chr () =
        if Random.int 8 = 5 then ' '
        else if Random.int 5 = 1 then
          Char.chr (97 + Random.int 26) |> Char.uppercase_ascii
        else Char.chr (97 + Random.int 26)
      in
      if length = 0 then acc
      else make_rnd_str (length - 1) (acc ^ String.make 1 (rand_chr ()))
    in
    make_rnd_str length ""
  in

  let rec random_parse_tests (tests : int) (acc : test list) =
    let test_string = make_rnd_str (Random.int 20 + 40) in
    if tests = 0 then acc
    else
      random_parse_tests (tests - 1)
        (parse_test ("parse " ^ test_string)
           ("\"" ^ test_string ^ "\"")
           (Rcp test_string)
        :: acc)
  in
  random_parse_tests tests []

let parse_float_tests =
  [
    parse_test "parse 1.1" "1.1" (Joul 1.1);
    parse_test "parse 0.0" "0.0" (Joul 0.0);
    parse_test "parse 100.001" "100.001" (Joul 100.001);
    parse_test "parse 12345.12345" "12345.12345" (Joul 12345.12345);
  ]

let parse_id_tests =
  [
    parse_test "parse n" "n" (Identifier "n");
    parse_test "parse x" "x" (Identifier "x");
    parse_test "parse func" "func" (Identifier "func");
    parse_test "parse this_is_an_identifier" "this_is_an_identifier"
      (Identifier "this_is_an_identifier");
  ]

let one_plus_one : Ast.expr = Ast.Binop (Ast.Add, Ast.Cal 1, Ast.Cal 1)

let one_plus_one_plus_one : Ast.expr =
  Ast.Binop (Ast.Add, one_plus_one, Ast.Cal 1)

let two_times_five : Ast.expr = Ast.Binop (Ast.Mult, Cal 2, Cal 5)

let two_times_five_plus_one : Ast.expr =
  Ast.Binop (Ast.Add, two_times_five, Cal 1)

let two_times_five_plus_one_plus_one_hundred : Ast.expr =
  Ast.Binop (Ast.Add, two_times_five_plus_one, Cal 100)

let n : Ast.expr = Identifier "n"
let one : Ast.expr = Cal 1
let n_plus_one : Ast.expr = Ast.(Binop (Add, n, one))

let parse_bop_tests =
  [
    parse_test "parse 1 + 1" "1 + 1" one_plus_one;
    parse_test "parse 1 + 1 + 1" "1 + 1 + 1" one_plus_one_plus_one;
    parse_test "parse 2 * 5" "2 * 5" two_times_five;
    parse_test "parse 2 * 5 + 1" "2 * 5 + 1" two_times_five_plus_one;
    parse_test "parse 2 * 5 + 1 + 100" "2 * 5 + 1 + 100"
      two_times_five_plus_one_plus_one_hundred;
  ]

let parse_let_tests =
  [
    parse_test "parse let n cook 1 in n + 1" "let n cook 1 in n + 1"
      (LetExpression ("n", one, n_plus_one));
    parse_test "parse\n  \n  let n cook 1 in\n  let m cook n in\n  m + n\n  "
      "\n  let n cook 1 in\n  let m cook n in\n  m + n\n  "
      Ast.(
        LetExpression
          ( "n",
            Cal 1,
            LetExpression
              ("m", Identifier "n", Binop (Add, Identifier "m", Identifier "n"))
          ));
    parse_test "parse\n  let a cook 100 in 1000\n  \n  "
      "let a cook 100 in 1000"
      Ast.(LetExpression ("a", Cal 100, Cal 1000));
  ]

let parse_function_tests =
  [
    parse_test "parse curry n cook n" "curry n cook n"
      Ast.(Function ("n", Identifier "n"));
    parse_test "parse curry n cook n + 1" "curry n cook n + 1"
      Ast.(Function ("n", Binop (Add, Identifier "n", Cal 1)));
    parse_test "parse curry argument cook true" "curry argument cook true"
      Ast.(Function ("argument", Bool true));
  ]

let parse_function_app_tests =
  [
    parse_test "parse f x" "f x"
      Ast.(FunctionApp (Identifier "f", Identifier "x"));
    parse_test "parse  n n" "n n"
      Ast.(FunctionApp (Identifier "n", Identifier "n"));
    parse_test "nested functions: parse f f f" "f f f"
      Ast.(
        FunctionApp
          (Identifier "f", FunctionApp (Identifier "f", Identifier "f")));
  ]

let parse_ternary_tests =
  [
    parse_test "parse if e_one then e_two else e_three"
      "if e_one then e_two else e_three"
      Ast.(
        Ternary (Identifier "e_one", Identifier "e_two", Identifier "e_three"));
    parse_test "nested expression"
      "\n\
      \    \n\
      \    if a then b\n\
      \    else if c then d\n\
      \    else e\n\
      \    \n\
      \    \n\
      \    "
      Ast.(
        Ternary
          ( Identifier "a",
            Identifier "b",
            Ternary (Identifier "c", Identifier "d", Identifier "e") ));
  ]

let complex_parse_tests =
  [
    parse_test "let with function" "let inc cook curry n cook n + 1 in inc 1"
      (Ast.LetExpression
         ( "inc",
           Function ("n", Binop (Add, Identifier "n", Cal 1)),
           FunctionApp (Identifier "inc", Cal 1) ));
    parse_test "three nested let expressions"
      "\n\
      \    \n\
      \    let a cook 1 in\n\
      \    let b cook 2 in\n\
      \    let c cook 3 in\n\
      \    4\n\
      \  \n\
      \    "
      Ast.(
        LetExpression
          ( "a",
            Cal 1,
            LetExpression ("b", Cal 2, LetExpression ("c", Cal 3, Cal 4)) ));
    parse_test "nested function with let"
      "\n\
      \    \n\
      \    curry n cook\n\n\
      \    let a cook curry n cook n + 1\n\n\
      \    in n\n\
      \    \n\
      \    \n\
      \    \n\
      \    "
      Ast.(
        Function
          ( "n",
            LetExpression
              ( "a",
                Function ("n", Binop (Add, Identifier "n", Cal 1)),
                Identifier "n" ) ));
  ]

let parse_tests =
  List.flatten
    [
      parse_int_tests;
      parse_bool_tests;
      parse_float_tests;
      parse_id_tests;
      parse_bop_tests;
      parse_let_tests;
      parse_function_tests;
      parse_function_app_tests;
      parse_ternary_tests;
      complex_parse_tests;
    ]

let random_tests =
  List.flatten
    [
      random_parse_string_tests number_of_random_tests;
      random_parse_int_tests number_of_random_tests;
      random_parse_float_tests number_of_random_tests;
      random_parse_binop_tests number_of_random_tests;
    ]

let tests = List.flatten [ eval_tests; parse_tests ]

let () =
  print_endline "\n\nRunning main test suite";
  run_test_tt_main ("suite" >::: tests)

let () =
  if run_random_tests then (
    print_endline "\nRunning random tests";
    Random.self_init ();
    run_test_tt_main ("random suite" >::: random_tests))
