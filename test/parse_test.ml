open Test_util
open Interp

let parse_int_tests =
  [
    parse_test "parse 1" "1" (Cal 1);
    parse_test "parse 0" "0" (Cal 0);
    parse_test "parse 12345" "12345" (Cal 12345);
    parse_test "parse ~1" "~1" (Unop (Unegation, Cal 1));
    parse_test "parse ~99999" "~99999" (Unop (Unegation, Cal 99999));
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
    parse_test "parse \"a a\"" "\"a a\"" (Rcp "a a");
    parse_test "parse \"this is a test\"" "\"this is a test\""
      (Rcp "this is a test");
    parse_test "parse \" \"" "\" \"" (Rcp " ");
  ]

let parse_float_tests =
  [
    parse_test "parse 1.1" "1.1" (Joul 1.1);
    parse_test "parse 0.0" "0.0" (Joul 0.0);
    parse_test "parse 100.001" "100.001" (Joul 100.001);
    parse_test "parse 12345.12345" "12345.12345" (Joul 12345.12345);
  ]

let parse_bowl_tests =
  [
    parse_test "[  ]  should parse to []" " [  ]  " (Bowl Nil);
    parse_test "[4] should parse to Bowl(Binop(Cons, Cal 4, Nil))" "[4]"
      (Bowl (Binop (Cons, Cal 4, Nil)));
    parse_test
      "[5, \"hi\" should parse to Bowl(Binop(Cons, Cal 5, (Cons, Rcp \"hi\", \
       Nil)))]"
      "[5, \"hi\"]"
      (Bowl (Binop (Cons, Cal 5, Binop (Cons, Rcp "hi", Nil))));
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

let read_file_tests = [ read_file_test "one" "prog_one" "let a cook 1" ]

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
      parse_bowl_tests;
      complex_parse_tests;
      read_file_tests;
    ]
