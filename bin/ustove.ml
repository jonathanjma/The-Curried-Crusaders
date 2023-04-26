open React
open Lwt
open LTerm_text
open Interp

module UStove = struct
  (* Type of the current state of the REPL *)
  type state = {
    count : int;
    command : string;
    env : Env.t;
    mode : int;
  }

  (* Initial state of the REPL *)
  let initial_state = { count = 1; command = ""; env = Env.empty; mode = 0 }

  (* Take a command from the REPL *)
  let rec take_commands state input =
    match input with
    | "" -> (state, "")
    | input -> (
        let state' = { state with count = state.count + 1; command = input } in
        (* evaluate ustove commands *)
        if String.get input 0 = '#' then
          ustove_commands state' (String.sub input 1 (String.length input - 1))
        else
          (* parse command *)
          match Interp.Main.parse input with
          | exception e -> (state', "  Error:" ^ Printexc.to_string e)
          | parsed ->
              (* show pretty printed AST *)
              let ast_pp =
                if state'.mode mod 2 = 0 then
                  Interp.Print.pretty_print parsed 1
                  ^ if state'.mode < 2 then "\n" else ""
                else ""
              in
              (* show evaluated result *)
              let eval =
                if state'.mode < 2 then "  " ^ show_eval parsed else ""
              in
              (state', ast_pp ^ eval))

  (* Evaulate a command *)
  and show_eval parsed =
    match Interp.Main.(parsed |> eval_wrapper |> string_of_val) with
    | exception e -> "Error:" ^ Printexc.to_string e
    | evaluated -> evaluated

  (* Evaluate a ustove command *)
  and ustove_commands state = function
    | "quit" -> Stdlib.exit 0
    | "ustove_help" ->
        ( state,
          "ustove defines the following directives:\n"
          ^ "#ustove_help : list all directives \n\
             #quit : quits session \n\
             #mode [x] : changes display mode (0 = evaluate & parse, 1 = \
             evaluate, 2 = parse)\n\
             #env : prints the current evaluation environment \n\
             #clear : clears all text from the screen\n\
             #re : relaunches ustove" )
    | "mode 0" -> ({ state with mode = 0 }, "mode 0 set")
    | "mode 1" -> ({ state with mode = 1 }, "mode 1 set")
    | "mode 2" -> ({ state with mode = 2 }, "mode 2 set")
    | "env" -> (state, Env.to_string state.env)
    | "clear" ->
        let _ = Sys.command "clear" in
        (state, "ustove cleared.\nPlease enter a command.")
    | "re" ->
        let _ = Sys.command "make ustove" in
        (state, "")
    | _ -> (state, "Invalid command.")
end

(* Adopted from lambda-term repl example:
   https://github.com/ocaml-community/lambda-term/blob/master/examples/repl.ml *)

let make_prompt _state =
  let prompt = "> " in
  eval [ S prompt ]

let make_output _state out =
  let output = if out = "" then "" else Printf.sprintf "%s\n\n" out in
  eval [ S output ]

class read_line ~term ~history ~state =
  object (self)
    inherit LTerm_read_line.read_line ~history ()
    inherit [Zed_string.t] LTerm_read_line.term term
    initializer self#set_prompt (S.const (make_prompt state))
  end

let rec loop term history state =
  Lwt.catch
    (fun () ->
      let rl =
        new read_line ~term ~history:(LTerm_history.contents history) ~state
      in
      rl#run >|= fun command -> Some command)
    (function
      | Sys.Break -> return None
      | exn -> Lwt.fail exn)
  >>= function
  | Some command ->
      let command_utf8 = Zed_string.to_utf8 command in
      let state, out = UStove.take_commands state command_utf8 in
      LTerm.fprints term (make_output state out) >>= fun () ->
      LTerm_history.add history command;
      loop term history state
  | None -> loop term history state

let main () =
  LTerm_inputrc.load () >>= fun () ->
  Lwt.catch
    (fun () ->
      let state = UStove.initial_state in
      Lazy.force LTerm.stdout >>= fun term ->
      loop term (LTerm_history.create []) state)
    (function
      | LTerm_read_line.Interrupt -> Lwt.return ()
      | exn -> Lwt.fail exn)

let () =
  print_endline "\n\nWelcome to ustove!";
  print_endline "Type #ustove_help for help about using ustove.";
  print_endline "Please enter a command.";
  Lwt_main.run (main ())
