{
  open Parser
}

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
  | int { CAL (int_of_string (Lexing.lexeme lexbuf))}
  | eof { EOF } 