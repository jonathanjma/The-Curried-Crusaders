{
  open Parser
}

let white = [' ' '\t']+
let number = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

let cal = '-'? number+
let joul = number+'.'number+
(* let ing =  *)
let rcp = letter+

rule read = 
  parse 
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "[" { LBRAC }
  | "]" { RBRAC }
  | "," { COMMA }
  | rcp { RCP (Lexing.lexeme lexbuf) }
  | cal { CAL (int_of_string (Lexing.lexeme lexbuf)) }
  | joul { JOUL (float_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF } 