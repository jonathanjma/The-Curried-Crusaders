(* mode = 0 - evaluate and parse; mode = 1 - evaluate, no parse; mode = 2 -
   parse only *)

let rec take_commands mode =
  print_string "> ";
  match read_line () with
  | exception End_of_file -> Stdlib.exit 0
  | "" -> take_commands mode
  | input -> (
      if String.get input 0 = '#' then
        ustring_commands mode (String.sub input 1 (String.length input - 1))
      else
        match Interp.Main.parse input with
        | exception e ->
            print_string "Error: ";
            print_endline (Printexc.to_string e);
            print_endline "";
            take_commands mode
        | parsed ->
            (* show pretty printed AST *)
            if mode mod 2 = 0 then print_endline "-------AST-------";
            print_endline (Interp.Print.pretty_print parsed 1);
            (* show evaluated result *)
            if mode < 2 then show_eval parsed;
            print_endline "";
            take_commands mode)

and show_eval parsed =
  match Interp.Main.(parsed |> eval_wrapper |> string_of_val) with
  | exception ex ->
      print_string "Error: ";
      print_endline (Printexc.to_string ex)
  | evaluated -> print_endline evaluated

and ustring_commands p = function
  | "quit" -> Stdlib.exit 0
  | "ustove_help" ->
      print_endline "\nustove defines the following directives:";
      print_string
        "#ustove_help : list all directives \n\
         #quit : quits session \n\
         #mode [x] : changes display mode (0 = evaluate & parse, 1 = evaluate, \
         2 = parse)\n\
         #clear : clears all text from the screen\n\
         #re : relaunches ustove \n\n";
      take_commands p
  | "mode 0" ->
      print_endline "mode 0 set\n";
      take_commands 0
  | "mode 1" ->
      print_endline "mode 1 set\n";
      take_commands 1
  | "mode 2" ->
      print_endline "mode 2 set\n";
      take_commands 2
  | "clear" ->
      let _ = Sys.command "clear" in
      print_endline "ustove cleared.\nPlease enter a command.";
      take_commands 2
  | "re" ->
      let _ = Sys.command "make ustove" in
      print_endline ""
  | _ ->
      print_endline "Invalid command.";
      take_commands p

let main () =
  print_endline "\n\nWelcome to ustove!";
  print_endline "Type #ustove_help for help about using ustove.";
  print_endline "Please enter a command.";
  take_commands 0

let () = main ()
