open OUnit2
open Interp
open Main

(* Random tests are performance intensive, you may not want to run them every
   time! *)
let run_random_tests = true

(* This number denotes how many of each type of random test to generate *)
let number_of_random_tests = 500

let tests = List.flatten [ Eval_test.eval_tests; Parse_test.parse_tests ]

let () =
  print_endline "\n\nRunning main test suite";
  run_test_tt_main ("suite" >::: tests)

let () =
  if run_random_tests then (
    print_endline "\nRunning random tests";
    Random.self_init ();
    run_test_tt_main ("random suite" >::: Random_test.random_tests number_of_random_tests))
