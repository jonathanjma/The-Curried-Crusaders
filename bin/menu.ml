open Interp

let rec parse_lines exprs =
  match exprs with
  | [] -> ()
  | expr :: t ->
      (match Main.(expr |> eval_wrapper_file) with
      | exception e -> print_endline ("Error:" ^ Printexc.to_string e)
      | evaluated ->
          let side_effects = Main.get_side_effects () in
          if String.length side_effects > 0 then print_endline side_effects
          else ());
      parse_lines t

let rec prompt_input () =
  (print_endline "\nEnter the path to the file you want to run.";
   print_string "> ";
   try
     let file_name = read_line () in
     let file_contents = Filereader.read ("test/programs/" ^ file_name) in
     match Main.parse_file file_contents with
     | exception e -> print_endline ("Error: " ^ Printexc.to_string e)
     | expr_list -> parse_lines expr_list
   with Filereader.InvalidDir -> print_endline "Error: File not found");
  prompt_input ()

let () = prompt_input ()
