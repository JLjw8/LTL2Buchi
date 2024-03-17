open Ast

exception ParserError of string

(** [parse s] parses [s] into an AST. Raises exception when there is a parser
    error*)
let parse (s : string) : formula =
  try
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.read lexbuf in
    ast
  with Parsing.Parse_error ->
    raise (ParserError "Parser error: Unexpected input syntax for LTL")

(** [trans_string fm] translates a parsed formula into a string. This is a
    helper function to be used during debug and testing*)
let rec trans_string (fm : formula) : string =
  let to_paren s = "(" ^ s ^ ")" in
  match fm with
  | Bool b -> "pbool*" ^ string_of_bool b
  | AP s -> "pstr*" ^ s
  | LOP_bin (logic_bin, f1, f2) ->
      let logic_bin_str =
        match logic_bin with
        | And -> "plb*AND"
        | Or -> "plb*OR"
        | Implies -> "plb*IMPLIES"
        | Eqv -> "plb*EQUAL"
      in
      to_paren (trans_string !f1 ^ " " ^ logic_bin_str ^ " " ^ trans_string !f2)
  | LOP_un (logic_un, f) ->
      let logic_un_str =
        match logic_un with
        | Neg -> "plu*NOT"
      in
      logic_un_str ^ " " ^ trans_string !f
  | TOP_bin (f1, temporal_bin, f2) ->
      let temporal_bin_str =
        match temporal_bin with
        | Until -> "ptb*UNTIL"
        | Release -> "ptb*RELEASE"
        | WeakU -> "ptb*WEAKU"
      in
      to_paren
        (trans_string !f1 ^ " " ^ temporal_bin_str ^ " " ^ trans_string !f2)
  | TOP_un (temporal_un, f) ->
      let temporal_un_str =
        match temporal_un with
        | Finally -> "ptu*FINALLY"
        | Always -> "ptu*ALWAYS"
      in
      temporal_un_str ^ " " ^ trans_string !f

(**[parse_show s] takes a natrual language sentence as an input, and output a
   string that shows parsed LTL data structure*)
let parse_show (s : string) : string = parse s |> trans_string
