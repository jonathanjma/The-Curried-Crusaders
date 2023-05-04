open Interp

let rec parse_lines lines =
  match lines with
  | [] -> ()
  | h :: t -> (
      match Main.parse h with
      | exception e -> print_endline ("Error: " ^ Printexc.to_string e)
      | parsed ->
          (match Main.(parsed |> eval_wrapper |> string_of_val) with
          | exception e -> print_endline ("Error:" ^ Printexc.to_string e)
          | evaluated -> print_endline evaluated);
          parse_lines t)

let rec prompt_input () =
  (print_endline "\nEnter the path to the file you want to run.";
   print_string "> ";
   try
     let file_name = read_line () in
     let file_contents = Filereader.read ("test/programs/" ^ file_name) in
     let lines = String.split_on_char '\n' file_contents in
     parse_lines lines
   with Filereader.InvalidDir -> print_endline "Error: File not found");
  prompt_input ()

let () = prompt_input ()
