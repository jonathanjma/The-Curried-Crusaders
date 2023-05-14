open Interp

type state_type = {
  ctr : int;
  mode : int;
  prob : int;
}

let rec generate_command state =
  Random.self_init ();
  match Random.int (max 3 state.prob) with
  | 0 ->
      let a1, a2 = (Random.int 100, Random.int 100) in
      let str =
        "let x cook " ^ string_of_int a1 ^ " in " ^ "x + " ^ string_of_int a2
      in
      if state.mode = 0 then (str, a1 + a2)
      else (Print.pretty_print (Main.parse str) 1, a1 + a2)
  | 1 | 2 ->
      let a1 = Random.int 100 in
      let txt, v = generate_expr "x" a1 state in
      let str = "let x cook " ^ string_of_int a1 ^ " in " ^ txt in
      if state.mode = 0 then (str, v)
      else (Print.pretty_print (Main.parse str) 1, v)
  | 3 | 4 | 5 ->
      let a1 = Random.int 100 in
      let a, b =
        generate_command { state with mode = 0; prob = max 3 state.prob - 1 }
      in
      let str = "let x cook " ^ string_of_int a1 ^ " in " ^ a in
      if state.mode = 0 then (str, b)
      else (Print.pretty_print (Main.parse str) 1, b)
  | 6 | 7 ->
      let a1 = Random.int 100 in
      let a, b =
        generate_command { state with mode = 0; prob = max 3 state.prob - 1 }
      in
      let str = "let x cook " ^ string_of_int a1 ^ " in (" ^ a ^ ") + x" in
      if state.mode = 0 then (str, a1 + b)
      else (Print.pretty_print (Main.parse str) 1, a1 + b)
  | 8 ->
      let a1 = Random.int 10 + 1 in
      let a, b =
        generate_command { state with mode = 0; prob = max 3 state.prob - 1 }
      in
      let str = "let x cook " ^ string_of_int a1 ^ " in (" ^ a ^ ") % x" in
      if state.mode = 0 then (str, b mod a1)
      else (Print.pretty_print (Main.parse str) 1, b mod a1)
  | _ ->
      let a1 = Random.int 100 in
      let a, b =
        generate_command { state with mode = 0; prob = max 3 state.prob - 1 }
      in
      let str = "let x cook " ^ string_of_int a1 ^ " in (" ^ a ^ ") * x" in
      if state.mode = 0 then (str, a1 * b)
      else (Print.pretty_print (Main.parse str) 1, a1 * b)

and generate_expr var_name var_val state =
  Random.self_init ();
  match Random.int 10 with
  | 0 ->
      let a2 = Random.int 100 in
      ( "if " ^ var_name ^ " > " ^ string_of_int a2 ^ " then 2 else 1",
        if var_val > a2 then 2 else 1 )
  | 1 ->
      let a2 = Random.int 100 in
      (var_name ^ " + " ^ string_of_int a2, var_val + a2)
  | 2 ->
      let a2 = Random.int 100 in
      (var_name ^ " * " ^ string_of_int a2, var_val * a2)
  | 3 ->
      let a2 = Random.int 15 + 1 in
      (var_name ^ " % " ^ string_of_int a2, var_val mod a2)
  | 4 ->
      let a2 = Random.int 50 in
      (var_name ^ " - " ^ string_of_int a2, var_val - a2)
  | 5 ->
      let a2 = Random.int 100 in
      ( "if bs (" ^ var_name ^ " < " ^ string_of_int a2 ^ ") then 2 else 1",
        if not (var_val < a2) then 2 else 1 )
  | 6 ->
      let a2, a3 = (Random.int 100, Random.int 15 + 1) in
      ( "let y cook " ^ string_of_int a2 ^ " in let z cook " ^ string_of_int a3
        ^ " in (x + 100 - y) % z",
        (var_val + 100 - a2) mod a3 )
  | 7 ->
      let a2, a3 = (Random.int 100, Random.int 100) in
      ( "let y cook " ^ string_of_int a2 ^ " in let z cook " ^ string_of_int a3
        ^ " in if " ^ var_name ^ " > y & " ^ var_name ^ " < z then 2 else 1",
        if var_val > a2 && var_val < a3 then 2 else 1 )
  | 8 ->
      let a2, a3 = (Random.int 100, Random.int 100) in
      ( "let y cook " ^ string_of_int a2 ^ " in let z cook " ^ string_of_int a3
        ^ " in if " ^ var_name ^ " < y | " ^ var_name ^ " >= z then 2 else 1",
        if var_val < a2 || var_val >= a3 then 2 else 1 )
  | _ ->
      let a2 = Random.int 100 in
      ("let y cook " ^ string_of_int a2 ^ " in x + 2 * y", var_val + (2 * a2))

let rec take_commands state =
  print_endline "Guess the meal cooked by these ingredients! \n";
  let quest, res = generate_command state in
  let _ = print_endline ("Expression: \n" ^ quest ^ "\n") in
  match_input quest res state

and match_input quest res state =
  match read_line (print_string "> ") with
  | "" -> match_input quest res state
  | input ->
      if String.get input 0 = '#' then
        mealguess_commands state (String.sub input 1 (String.length input - 1))
      else if input = string_of_int res then (
        print_endline "Correct! You guessed the meal that was cooked! \n";
        let state' = { state with ctr = state.ctr + 1 } in
        let _ =
          print_endline
            ("You have " ^ string_of_int state'.ctr
            ^ if state'.ctr = 1 then " point." else " points.")
        in
        take_commands { state with ctr = state.ctr + 1 })
      else (
        print_endline
          ("Incorrect! \n\nThe correct answer is " ^ string_of_int res ^ ".\n");
        let state' = { state with ctr = max (state.ctr - 1) 0 } in
        print_endline
          ("You have " ^ string_of_int state'.ctr
          ^ if state'.ctr = 1 then " point." else " points.");
        take_commands state')

and mealguess_commands state inp =
  match inp with
  | "quit" -> Stdlib.exit 0
  | "mg_help" ->
      print_endline
        "To play Guess the Meal, enter the corresponding value that \
         corresponds to the meal (expression) that was cooked (evaluated).\n\
         For instance, if given the expression \"let x cook 1 in x + 1\", \
         answering \"2\" would give you a point. If you answer incorrectly, \
         you will lose a point.\n\n\
         Try to get as many points as possible to be a professional iCook Chef! \n";
      take_commands state
  | "reset" ->
      let _ = Sys.command "clear" in
      print_endline "Points reset. \n";
      take_commands { ctr = 0; mode = 0; prob = 7 }
  | "cmds" ->
      print_endline
        "MealGuess has the following commands: \n\
         #quit : quits session \n\
         #mg_help : provides information on how to play MealGuess \n\
         #reset : resets point count and clears screen \n\
         #mode [x] : changes display mode (0 = iCook expression, 1 = pretty \
         printed parse)\n\
         #difficulty [x] : changes difficulty mode (0 = easy (no recursion), 1 \
         = normal (some recursion and scopes) - default, 2 = hard (a lot of \
         recursion and scopes))";
      take_commands state
  | "mode 0" ->
      print_endline "mode 0 set";
      take_commands { state with mode = 0 }
  | "mode 1" ->
      print_endline "mode 1 set";
      take_commands { state with mode = 1 }
  | "difficulty 0" ->
      print_endline "easy difficulty set";
      take_commands { state with prob = 3 }
  | "difficulty 1" ->
      print_endline "normal difficulty set";
      take_commands { state with prob = 7 }
  | "difficulty 2" ->
      print_endline "hard difficulty set";
      take_commands { state with prob = 15 }
  | _ ->
      print_endline "Invalid command. \n";
      take_commands state

let () =
  print_endline "\n\nWelcome to MealGuess!";
  print_endline "Type #mg_help for help about playing MealGuess.";
  print_endline "Type #cmds to see all of MealGuess commands.";
  let state = { ctr = 0; mode = 0; prob = 7 } in
  take_commands state
