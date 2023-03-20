let rec take_commands () =
  print_string "> ";
  match read_line () with
  | "" -> take_commands ()
  | x -> (
      if String.get x 0 = '#' then
        ustring_commands (String.sub x 1 (String.length x - 1))
      else
        match
          let parsed = Interp.Main.parse x in
          Interp.Main.pretty_print parsed 1
        with
        | exception End_of_file -> ()
        | exception e ->
            print_string "Error: ";
            print_endline (Printexc.to_string e);
            print_endline "";
            take_commands ()
        | x ->
            print_endline x;
            print_endline "";
            take_commands ())

and ustring_commands = function
  | "quit" -> Stdlib.exit 0
  | "ustove_help" ->
      print_endline "\nustove defines the following directives:";
      print_string
        "#ustove_help : list all directives \n#quit : quits session \n\n";
      take_commands ()
  | _ ->
      print_endline "Invalid command.";
      take_commands ()

let main () =
  print_endline "\n\nWelcome to ustove!";
  print_endline "Type #ustove_help for help about using ustove.";
  print_endline "Please enter a command.";
  take_commands ()

let () = main ()
