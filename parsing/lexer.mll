{
open Parser
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter+

rule read = 
  parse
  | white { read lexbuf }
  | " is true" { read lexbuf}
  | ( "true" |  "True" |  "TRUE" ) { BOOL (bool_of_string "true") }
  | ( "false" | "False" | "FALSE" ) { BOOL (bool_of_string "false")}
  | "NOT" | "not" | "!" | "no" | "No" | "Not" | "NO"  { NEG }
  | "&" | "and" | "AND"| "And" { AND }
  | "or" | "|" | "OR" | "Or" { OR }
  | "=>"| "implies" | "IMPLIES"| "Implies" { IMPLIES }
  | "=" | "equivalent" | "equals"| "Equals" | "EQUALS" { EQV }
  | "Finally" | "finally" | "FINALLY" | "Final" | "final" { FINALLY }
  | "Until" | "until" | "UNTIL" | "til" | "Til" { UNTIL }
  | "Always" | "always" | "ALWAYS" | "forever" | "Forever" { ALWAYS }
  | "Release" | "release" | "RELEASE" { RELEASE }
  | "Weak_until" | "weak_until" {WEAKU}
  | "(" { LPAREN }
  | ")" { RPAREN }
  | id { APR (Lexing.lexeme lexbuf)}
  | eof {EOF}