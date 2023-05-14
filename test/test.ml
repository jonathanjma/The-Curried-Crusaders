(** Testing Plan:

    The first part of our testing plan is unit testing. In eval_test.ml, we
    implemented several functions and lists of unit tests that verify the
    correctness of evaluating specific iCook programs. This is one of the ways
    we implemented black box testing. Additionally, in parse_test.ml, we created
    unit tests to verify that specific iCook programs parse to the correct
    abstract syntax tree representation. This is one of the ways we implemented
    glass box testing.

    The second part of our testing plan is randomized testing. In this module,
    implemented in random_test.ml, random inputs are injected into template
    programs, which are then verified to have output parameterized on the random
    value. Since this method of testing requires the constrution and
    verification of ASTs, this is one of the ways we implemented glass box
    testing.

    The first type of random test is random function testing, where a random
    integer or string is generated, then inserted into the a function
    definition. After that, the expected value of the generated unit test is
    calculated based on a predetermined formula and the randomly generated
    integer or string. This method of testing does not require knowledge of the
    internal mechanisms of the interpreation system, so it is a black box
    testing method.

    The second type of random test is tests in which we verify that integers and
    strings parse correctly. This is a black box testing system.

    The third type of random test is tests in which we verify that randomly
    generated integers and strings are correctly bound in the environment in let
    expressions, then the let expressions evaluate to the randomly generated
    values. This is a glass box testing method, since checking whether the
    binding was correctly placed into the environment requires knowledge of the
    internal mechanisms.

    The third part of our testing plan is manual testing. Using our REPL,
    ustove, we can test iCook by typing in various expressions and checking to
    see if they evaulate to the correct value and AST. Together, these three
    parts of our testing plan ensure the correctness of the iCook programming
    language. *)

open OUnit2
open Interp
open Main

(* Random tests are performance intensive, you may not want to run them every
   time! *)
let run_random_tests = false
let tests = List.flatten [ Eval_test.eval_tests; Parse_test.parse_tests ]

let () =
  print_endline "\n\nRunning main test suite";
  run_test_tt_main ("suite" >::: tests)

let () =
  if run_random_tests then (
    print_endline "\nRunning random tests";
    Random.self_init ();
    run_test_tt_main ("random suite" >::: Random_test.random_tests))
