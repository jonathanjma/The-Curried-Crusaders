open Test_util
open Interp
open Main
open OUnit2

module RandomFunctionTests = struct
  type function_input =
    | Int of int
    | String of string
    | Bool of bool

  let func_1 : string = {|curry n cook n|}
  let expectation_function_1 (x : int) = x
  let func_2 : string = {|curry n cook n + 1|}
  let expectation_function_2 (x : int) = x + 1
  let func_3 : string = {|curry n cook n + 10|}
  let expectation_function_3 (x : int) = x + 10
  let func_4 : string = {|curry n cook n * 2|}
  let expectation_function_4 (x : int) = x * 2
  let func_5 : string = {|curry n cook 5 * n|}
  let expectation_function_5 (x : int) = 5 * x
  let func_6 : string = {|curry n cook 3 * n + 1|}
  let expectation_function_6 (x : int) = (3 * x) + 1
  let func_7 : string = {|curry n cook (1 + n) * n|}
  let expectation_function_7 (x : int) = (1 + x) * x

  let string_of_function_input : function_input -> string = function
    | Int n -> string_of_int n
    | String s -> s
    | Bool b -> string_of_bool b

  let random_input_test (x : function_input) (function_string : string)
      (expected_output : string) =
    let expression : string =
      {|let n cook |} ^ string_of_function_input x ^ {|in |} ^ "let f cook "
      ^ function_string ^ "in f n"
    in
    eval_autonamed_string_expression_test expected_output expression

  let random_int_tests (func : string) (expectation_function : int -> int) =
    let tests : test list ref = ref [] in
    for i = 0 to 500 do
      let x : int = Random.int 200 in
      tests :=
        random_input_test (Int x) func_1
          (x |> expectation_function |> string_of_int)
        :: !tests
    done;
    !tests

  let tests : test list =
    List.flatten
      [
        random_int_tests func_1 expectation_function_1;
        random_int_tests func_2 expectation_function_2;
        random_int_tests func_3 expectation_function_3;
        random_int_tests func_4 expectation_function_4;
        random_int_tests func_5 expectation_function_5;
        random_int_tests func_6 expectation_function_6;
        random_int_tests func_7 expectation_function_7;
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

let random_tests num =
  List.flatten
    [
      random_parse_string_tests num;
      random_parse_int_tests num;
      random_parse_float_tests num;
      random_parse_binop_tests num;
      random_eval_comparison_tests num;
      random_eval_binop_tests num;
      random_let_expr_test num;
      random_let_def_test num (* RandomFunctionTests.tests; *);
    ]
