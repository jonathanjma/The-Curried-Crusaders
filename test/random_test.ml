open Test_util
open Interp
open Main
open OUnit2

(* This number denotes how many of each type of random test to generate *)
let number_of_random_tests = 500

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

let rec random_let_expr_test num = let_expr_test_tr num []

and let_expr_test_tr num acc =
  if num = 0 then acc
  else
    let rand_int = Random.int 229 in
    let to_test = "let x cook " ^ string_of_int rand_int ^ " in x" in
    let_expr_test_tr (num - 1)
      (eval_autonamed_string_expression_test (string_of_int rand_int) to_test
      :: acc)

let rec random_let_def_test num = let_expr_test_tr num []

and let_def_test_tr num acc =
  if num = 0 then acc
  else
    let rand_int = Random.int Int.max_int in
    let to_test = "let x cook " ^ string_of_int rand_int in
    let_def_test_tr (num - 1)
      (env_equality_test (Env.add_binding "x" (Cal rand_int) Env.empty) to_test
      :: acc)

let rec random_eval_comparison_tests (tests : int) : test list =
  let rec random_eval_comparison_tests_tr (tests : int) (acc : test list) :
      test list =
    if tests = 0 then acc
    else
      let n1 =
        if Random.int 2 = 0 then ~-(Random.int 229) else Random.int 229
      in
      let n2 =
        if Random.int 2 = 0 then ~-(Random.int 229) else Random.int 229
      in
      let binop =
        match Random.int 5 with
        | 0 -> ">"
        | 1 -> "<"
        | 2 -> ">="
        | 3 -> "<="
        | 4 -> "="
        | _ -> failwith "Poorly generated random number."
      in
      let e1 = icook_str_of_int n1 ^ binop ^ icook_str_of_int n2 in
      let expected =
        string_of_bool
          (match binop with
          | ">" -> n1 > n2
          | "<" -> n1 < n2
          | ">=" -> n1 >= n2
          | "<=" -> n1 <= n2
          | "=" -> n1 = n2
          | _ -> failwith "Poorly generated binop")
      in
      random_eval_comparison_tests_tr (tests - 1)
        (eval_autonamed_string_expression_test expected e1 :: acc)
  in
  random_eval_comparison_tests_tr tests []

let random_eval_binop_tests (tests : int) : test list =
  let rec random_eval_binop_tests_tr (tests : int) (acc : test list) : test list
      =
    if tests = 0 then acc
    else
      let n1 =
        if Random.int 2 = 0 then ~-(Random.int 229) else Random.int 229
      in
      let n2 =
        if Random.int 2 = 0 then ~-(Random.int 229) else Random.int 229
      in
      let binop =
        match Random.int 5 with
        | 0 -> "+"
        | 1 -> "-"
        | 2 -> "/"
        | 3 -> " fk "
        | 4 -> "*"
        | _ -> failwith "Poorly generated random number."
      in
      let e1 = icook_string_of_int n1 ^ binop ^ icook_string_of_int n2 in
      let expected =
        string_of_int
          (match binop with
          | "+" -> n1 + n2
          | "-" -> n1 - n2
          | "/" -> n1 / n2
          | " fk " -> Int.logxor n1 n2
          | "*" -> n1 * n2
          | _ -> failwith "Poorly generated binop")
      in
      random_eval_binop_tests_tr (tests - 1)
        (eval_autonamed_string_expression_test expected e1 :: acc)
  in
  random_eval_binop_tests_tr tests []

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
          (a ^ " " ^ Ast.string_of_bop bop ^ " " ^ b)
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
      random_let_def_test number_of_random_tests;
      RandomFunctionTests.tests;
    ]
