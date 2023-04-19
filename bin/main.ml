(* p = 0 - evaluate and parse; p = 1 - evaluate, no parse; p = 2 - parse only *)
let rec take_commands p =
  print_string "> ";
  match read_line () with
  | "" -> take_commands p
  | x -> (
      if String.get x 0 = '#' then
        ustring_commands p (String.sub x 1 (String.length x - 1))
      else
        match
          let parsed = Interp.Main.parse x in
          Interp.Main.pretty_print parsed 1
        with
        | exception End_of_file -> Stdlib.exit 0
        | exception e ->
            print_string "Error: ";
            print_endline (Printexc.to_string e);
            print_endline "";
            take_commands p
        | y ->
            if p mod 2 = 0 then print_endline y;
            if p < 2 then show_eval (Interp.Main.parse x);
            print_endline "";
            take_commands p)

and show_eval prs =
  match Interp.Main.(pretty_print (eval prs) 1) with
  | exception End_of_file -> Stdlib.exit 0
  | exception e ->
      print_string "Error: ";
      print_endline (Printexc.to_string e)
  | x -> print_endline x

and ustring_commands p = function
  | "quit" -> Stdlib.exit 0
  | "ustove_help" ->
      print_endline "\nustove defines the following directives:";
      print_string
        "#ustove_help : list all directives \n\
         #quit : quits session \n\
         #mode [x] : changes display mode (0 = evaluate & parse, 1 = evaluate, \
         2 = parse)\n\
         #clear : clears all text from the screen \n\n";
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
