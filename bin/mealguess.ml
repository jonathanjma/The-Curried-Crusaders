type state_type = {
  ctr : int;
  mode : int;
}

let rec generate_command () =
  Random.self_init ();
  match Random.int 5 with
  | 0 ->
      let a1, a2 = (Random.int 100, Random.int 100) in
      ( "let x cook " ^ string_of_int a1 ^ " in (" ^ "x + " ^ string_of_int a2
        ^ ")",
        a1 + a2 )
  | 1 ->
      let a1 = Random.int 100 in
      let txt, v = generate_expr "x" a1 in
      ("let x cook " ^ string_of_int a1 ^ " in " ^ txt, v)
  | _ ->
      let a1 = Random.int 100 in
      let a, b = generate_command () in
      ("let x cook " ^ string_of_int a1 ^ " in " ^ a, b)

and generate_expr var_name var_val =
  Random.self_init ();
  match Random.int 5 with
  | 0 ->
      let a2 = Random.int 100 in
      ( "if " ^ var_name ^ " > " ^ string_of_int a2 ^ " then 1 else 0",
        if var_val > a2 then 1 else 0 )
  | 1 ->
      let a2 = Random.int 100 in
      (var_name ^ " + " ^ string_of_int a2, var_val + a2)
  | 2 ->
      let a2 = Random.int 100 in
      (var_name ^ " * " ^ string_of_int a2, var_val * a2)
  | 3 ->
      let a2 = Random.int 15 in
      (var_name ^ " % " ^ string_of_int a2, var_val mod a2)
  | _ ->
      let a2 = Random.int 50 in
      (var_name ^ " - " ^ string_of_int a2, var_val - a2)

let rec take_commands state =
  print_endline "Guess the meal! \n";
  let quest, res = generate_command () in
  let _ = print_endline ("Expression: " ^ quest) in
  match read_line (print_string "> ") with
  | input ->
      if String.get input 0 = '#' then
        mealguess_commands state (String.sub input 1 (String.length input - 1))
      else if input = string_of_int res then (
        print_endline "Correct! \n";
        let state' = { state with ctr = state.ctr + 1 } in
        let _ =
          print_endline
            ("You have " ^ string_of_int state'.ctr
            ^ if state'.ctr = 1 then " point." else " points.")
        in
        take_commands state')
      else (
        print_endline
          ("Incorrect! \n\n The correct answer is " ^ string_of_int res ^ ".\n");
        print_endline
          ("You have " ^ string_of_int state.ctr
          ^ if state.ctr = 1 then " point." else " points.");
        take_commands state)

and mealguess_commands state = function
  | "quit" -> Stdlib.exit 0
  | "mg_help" ->
      print_endline
        "To play Guess the Meal, enter the corresponding value that \
         corresponds to the meal (expression) that was cooked (evaluated).\n\
         For instance, if given the expression \"let x cook 1 in x + 1\", \
         answering \"2\" would give you a point.\n\n\
         Try to get as many points as possible to be a professional iCook Chef! \n";
      take_commands state
  | "reset" ->
      let _ = Sys.command "clear" in
      print_endline "Points reset. \n";
      take_commands { ctr = 0; mode = 0 }
  | "cmds" ->
      print_endline
        "MealGuess has the following commands: \n\
         #quit : quits session \n\
         #mg_help : provides information on how to play MealGuess \n\
         #reset : resets point count and clears screen \n"
  | _ ->
      print_endline "Invalid command. \n";
      take_commands state

let () =
  print_endline "\n\nWelcome to MealGuess!";
  print_endline "Type #mg_help for help about playing MealGuess.";
  print_endline "Type #cmds to see all of MealGuess commands.";
  let state = { ctr = 0; mode = 0 } in
  take_commands state
