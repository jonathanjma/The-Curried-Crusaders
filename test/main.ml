open OUnit2
open Interp
open Main

(* Random tests are performance intensive, you may not want to run them every
   time! *)
let run_random_tests = false

(* This number denotes how many of each type of random test to generate *)
let number_of_random_tests = 500
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
    (string_expression ^ " should parse to " ^ expected_output)
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

module RandomFunctionTests = struct
  type function_input =
    | Int of int
    | String of string

  let chars : string list =
    [
      "0";
      "1";
      "2";
      "3";
      "4";
      "5";
      "6";
      "7";
      "8";
      "9";
      "a";
      "b";
      "c";
      "d";
      "e";
      "f";
      "g";
      "h";
      "i";
      "j";
      "k";
      "l";
      "m";
      "n";
      "o";
      "p";
      "q";
      "r";
      "s";
      "t";
      "u";
      "v";
      "w";
      "x";
      "y";
      "z";
      "A";
      "B";
      "C";
      "D";
      "E";
      "F";
      "G";
      "H";
      "I";
      "J";
      "K";
      "L";
      "M";
      "N";
      "O";
      "P";
      "Q";
      "R";
      "S";
      "T";
      "U";
      "V";
      "W";
      "X";
      "Y";
      "Z";
    ]

  let rec find_list (index : int) (lst : 'a list) =
    match lst with
    | [] -> failwith "empty"
    | h :: t -> if index = 0 then h else find_list (index - 1) t

  let random_char () =
    let random_index : int = Random.int (List.length chars) in
    find_list random_index chars

  let random_string () =
    let length : int = Random.int 100 in
    let rec random_string (length : int) (acc : string) =
      match length with
      | 0 -> acc
      | _ ->
          let c : string = random_char () in
          random_string (length - 1) (acc ^ c)
    in

    random_string length ""

  let int_func_1 : string = {|curry n cook n|}
  let int_expectation_function_1 (x : int) = x
  let int_func_2 : string = {|curry n cook n + 1|}
  let int_expectation_function_2 (x : int) = x + 1
  let int_func_3 : string = {|curry n cook n + 10|}
  let int_expectation_function_3 (x : int) = x + 10
  let int_func_4 : string = {|curry n cook n * 2|}
  let int_expectation_function_4 (x : int) = x * 2
  let int_func_5 : string = {|curry n cook 5 * n|}
  let int_expectation_function_5 (x : int) = 5 * x
  let int_func_6 : string = {|curry n cook 3 * n + 1|}
  let int_expectation_function_6 (x : int) = (3 * x) + 1
  let int_func_7 : string = {|curry n cook (1 + n) * n|}
  let int_expectation_function_7 (x : int) = (1 + x) * x
  let string_func_1 : string = {|curry n cook n|}
  let string_expectation_function_1 (x : string) = x
  let string_func_2 : string = {|curry n cook n + "a"|}
  let string_expectation_function_2 (x : string) = x ^ "a"
  let string_func_3 : string = {|curry n cook n + "aa"|}
  let string_expectation_function_3 (x : string) = x ^ "aa"
  let string_func_4 : string = {|curry n cook "z" + n + "y"|}
  let string_expectation_function_4 (x : string) = "z" ^ x ^ "y"
  let add_quotes (x : string) = {|"|} ^ x ^ {|"|}

  let string_of_function_input : function_input -> string = function
    | Int n -> string_of_int n
    | String s -> {|"|} ^ s ^ {|"|}

  let random_input_test (x : function_input) (function_string : string)
      (expected_output : string) =
    let expression : string =
      "(" ^ function_string ^ ")" ^ " " ^ string_of_function_input x
    in
    eval_autonamed_string_expression_test expected_output expression

  let random_int_tests (func : string) (expectation_function : int -> int) =
    let tests : test list ref = ref [] in
    for i = 0 to number_of_random_tests do
      let x : int = Random.int 200 in
      tests :=
        random_input_test (Int x) func
          (x |> expectation_function |> string_of_int)
        :: !tests
    done;
    !tests

  let random_string_tests (func : string)
      (expectation_function : string -> string) =
    let tests : test list ref = ref [] in
    for i = 0 to number_of_random_tests do
      let x : string = random_string () in
      tests :=
        random_input_test (String x) func (expectation_function x |> add_quotes)
        :: !tests
    done;
    !tests

  let tests : test list =
    List.flatten
      [
        random_int_tests int_func_1 int_expectation_function_1;
        random_int_tests int_func_2 int_expectation_function_2;
        random_int_tests int_func_3 int_expectation_function_3;
        random_int_tests int_func_4 int_expectation_function_4;
        random_int_tests int_func_5 int_expectation_function_5;
        random_int_tests int_func_6 int_expectation_function_6;
        random_int_tests int_func_7 int_expectation_function_7;
        random_string_tests string_func_1 string_expectation_function_1;
        random_string_tests string_func_2 string_expectation_function_2;
        random_string_tests string_func_3 string_expectation_function_3;
        random_string_tests string_func_4 string_expectation_function_4;
      ]
end

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
    (* eval_float_expression_test "-7.0 should parse to 2.0" ~-.7.0 "-7.0"; *)
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
      ("so true that PI(E) = " ^ string_of_float Float.pi)
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
    ]

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

let string_of_bop = function
  | Ast.Add -> "+"
  | Ast.Mult -> "*"
  | Ast.Fork -> "fk"
  | Ast.Subtract -> "-"
  | Ast.Divide -> "/"
  | Ast.Cons -> "::"
  | Ast.Geq -> ">="
  | Ast.Leq -> "<="
  | Ast.Equal -> "="
  | Ast.Greater -> ">"
  | Ast.Less -> "<"

let random_parse_binop_tests (tests : int) =
  let rec random_parse_binop_tests (tests : int) (acc : test list) =
    if tests = 0 then acc
    else
      let bop : Ast.bop =
        match Random.int 5 with
        | 0 -> Add
        | 1 -> Mult
        | 2 -> Fork
        | 3 -> Divide
        | 4 -> Subtract
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

    let () =
      if false (* Random.bool () *) then test_float := -1. *. !test_float
    in

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
          ("parse " ^ icook_string_of_int !test_int)
          (icook_string_of_int !test_int)
          (if !test_int < 0 then Unop (Unegation, Cal (!test_int * -1))
          else Cal !test_int)
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
    RandomFunctionTests.random_input_test (Int 1) {|curry n cook n|} "1";
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

let random_tests =
  List.flatten
    [
      random_parse_string_tests number_of_random_tests;
      random_parse_int_tests number_of_random_tests;
      random_parse_float_tests number_of_random_tests;
      random_parse_binop_tests number_of_random_tests;
      random_eval_comparison_tests number_of_random_tests;
      random_eval_binop_tests number_of_random_tests;
      random_let_expr_test number_of_random_tests;
      random_let_def_test number_of_random_tests
      (* RandomFunctionTests.tests; *);
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
