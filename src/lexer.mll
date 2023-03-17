{
  open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

let cal = '-'? digit+
let joul = digit+'.'digit+
(* let ing =  *)
let rcp = letter+

let digit = ['0'-'9']
let int = '-'? digit+
let white = [' ' '\t']+




rule read =
  parse 
  | white { read lexbuf }
  | "+" { PLUS }
  | "*" { TIMES }
  | "fk" { FORK }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "[" { LBRAC }
  | "]" { RBRAC }
  | "," { COMMA }
  | "PIE" { PIE }
  | "cook" {COOK}
  | "curry" {CURRY}
  | "let" {LET}
  | "in" {IN}
  | rcp { RCP (Lexing.lexeme lexbuf) }
  | cal { CAL (int_of_string (Lexing.lexeme lexbuf)) }
  | joul { JOUL (float_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
  | "\"" {DOUBLE_QUOTE}