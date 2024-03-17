open Ltl2ba
open Ltl
open Buchi
open Ltl2buchi
open Visualizer
open Ltlparsing
open Random

exception InputError of string

(** [parse s] parses [s] into an AST. let parse (s : string) : Ast.formula = let
    lexbuf = Lexing.from_string s in let ast = Parser.prog Lexer.read lexbuf in
    ast *)

let convert_lb (lb : Ast.logic_bin) : Ltl.logic_bin =
  match lb with
  | And -> And
  | Or -> Or
  | Implies -> Implies
  | Eqv -> Eqv

let convert_lu (lu : Ast.logic_un) : Ltl.logic_un =
  match lu with
  | Neg -> Neg

let convert_tb (tb : Ast.temporal_bin) : Ltl.temporal_bin =
  match tb with
  | Until -> Until
  | Release -> Release
  | _ -> failwith "invalid"

let convert_tu (tu : Ast.temporal_un) : Ltl.temporal_un =
  match tu with
  | Finally -> Finally
  | _ -> failwith "invalid"

let rec convert_formula (af : Ast.formula) : Ltl.formula =
  match af with
  | Bool b -> Bool b
  | AP ap -> AP ap
  | LOP_bin (lb, f1, f2) ->
      let llb = convert_lb lb in
      let lf1 = convert_formula !f1 in
      let lf2 = convert_formula !f2 in
      LOP_bin (llb, ref lf1, ref lf2)
  | LOP_un (lu, f) ->
      let llu = convert_lu lu in
      let lf = convert_formula !f in
      LOP_un (llu, ref lf)
  | TOP_bin (f1, tb, f2) ->
      let ltb = convert_tb tb in
      let lf1 = convert_formula !f1 in
      let lf2 = convert_formula !f2 in
      TOP_bin (ref lf1, ltb, ref lf2)
  | TOP_un (tu, f) ->
      let ltu = convert_tu tu in
      let lf = convert_formula !f in
      TOP_un (ltu, ref lf)

(* A small repository of LTLs to demo if the user chooses so*)
let ltl1 = Ltl.Bool true
let ltl2 = Ltl.AP "phi"
let ltl3 = Ltl.AP "psi"
let ltl4 = Ltl.LOP_bin (And, ref ltl2, ref ltl3)
let ltl5 = Ltl.LOP_bin (Implies, ref ltl2, ref ltl3)
let ltl6 = Ltl.LOP_un (Neg, ref ltl5)
let ltl7 = Ltl.TOP_bin (ref ltl4, Until, ref ltl5)
let ltl8 = Ltl.TOP_un (Finally, ref ltl5)
let ltl9 = Ltl.LOP_un (Neg, ref ltl4)
let ltl10 = Ltl.TOP_bin (ref ltl2, Until, ref ltl3)

let ltl_list =
  Ltl.[ ltl1; ltl2; ltl3; ltl4; ltl5; ltl6; ltl7; ltl8; ltl9; ltl10 ]

let item_random l =
  let i = Random.int (List.length l) in
  List.nth l i

let help_yn_to_bool = function
  | "y" -> true
  | _ -> false

let help_hex_to_int hex_string =
  let n =
    let rec hex_char_to_int char =
      match char with
      | '0' .. '9' -> Char.code char - Char.code '0'
      | 'a' .. 'f' -> 10 + (Char.code char - Char.code 'a')
      | 'A' .. 'F' -> 10 + (Char.code char - Char.code 'A')
      | _ -> raise (InputError "Please just input six digits of 0-9 and A-F")
    in

    let rec convert_hex_string acc chars =
      match chars with
      | [] -> acc
      | char :: rest ->
          convert_hex_string ((acc * 16) + hex_char_to_int char) rest
    in

    let hex_chars = List.of_seq (String.to_seq hex_string) in
    convert_hex_string 0 hex_chars
  in
  if n >= 0 && n <= 16777215 then n
  else raise (InputError "Please just input six digits of 0-9 and A-F")

let help_check_hex_int n =
  if n >= 0 && n <= 16777215 then n
  else failwith "Please just input six digits of 0-9 and A-F"

(* Prompt the user for input and return the entered string *)
let prompt_user_ltl () =
  print_string
    "Type your LTL as a string. Press Enter when done. To use a random LTL \
     made by us, just press Enter: ";
  flush stdout;
  read_line ()

(* Prompt the user for initial state drawing style *)
let prompt_user_initial_state () =
  print_string
    "Do you want to mark the initial state with a separate box? Press y \
     followed by Enter, or anything else for no: ";
  flush stdout;
  read_line ()

(* Prompt the user for graph label (LTL) *)
let prompt_user_label_ltl () : string =
  print_string
    "Do you want to label the automaton with the string representation of your \
     LTL? Press y followed by Enter, or anything else for no: ";
  flush stdout;
  read_line ()

(* Prompt the user for graph label (custom) *)
let prompt_user_label_custom () : string =
  print_string
    "Do you want to label the automaton with a customized string? If yes, type \
     it, followed by Enter. If no, just press Enter: ";
  flush stdout;
  read_line ()

(* Prompt the user for whether to use transparent background *)
let prompt_user_background_trans () =
  print_string
    "Do you want the image background to be transparent? Press y followed by \
     Enter, or anything else for no: ";
  flush stdout;
  read_line ()

(* Prompt and parse the user for background color. If empty input, default
   16777215 white. Raise exception if illegal input *)
let rec prompt_parse_user_background_color () : int =
  try
    let s =
      print_string
        "What color do you want for background? Type it as hex RGB value (e.g. \
         FFFFFF for white, 000000 for black) followed by Enter. If you want \
         white, just press Enter: ";
      flush stdout;
      read_line ()
    in
    match s with
    | "" -> 16777215
    | ss -> help_hex_to_int ss
  with _ ->
    print_string "Your RGB input doesn't look right. Let's try again\n";
    prompt_parse_user_background_color ()

(* Prompt and parse the user for shape color. If empty input, default 0 black.
   Raise exception if illegal input *)
let rec prompt_parse_user_shape_color () : int =
  try
    let s =
      print_string
        "What color do you want for vertices and edges? Type it as hex RGB \
         value (e.g. FFFFFF for white, 000000 for black) followed by Enter. If \
         you want white, just press Enter: ";
      flush stdout;
      read_line ()
    in
    match s with
    | "" -> 0
    | ss -> help_hex_to_int ss
  with _ ->
    print_string "Your RGB input doesn't look right. Let's try again\n";
    prompt_parse_user_shape_color ()

(* Prompt and parse the user for font color. If empty input, default 0 black.
   Raise exception if illegal input *)
let rec prompt_parse_user_font_color () : int =
  try
    let s =
      print_string
        "What color do you want for text labels? Type it as hex RGB value \
         (e.g. FFFFFF for white, 000000 for black) followed by Enter. If you \
         want white, just press Enter: ";
      flush stdout;
      read_line ()
    in
    match s with
    | "" -> 0
    | ss -> help_hex_to_int ss
  with _ ->
    print_string "Your RGB input doesn't look right. Let's try again\n";
    prompt_parse_user_font_color ()

(* function to open file*)
let open_png_file filename =
  let command = Printf.sprintf "open %s" filename in
  let exit_code = Sys.command command in
  if exit_code <> 0 then Printf.eprintf "Failed to open the PNG file.\n%!"

let rec prompt_parse_convert () : Ltl.formula =
  try
    let user_input : string = prompt_user_ltl () in
    if user_input = "" then (
      let random_ltl = item_random ltl_list in
      Printf.printf "Your random LTL is %s\n" (Ltl.to_string random_ltl);
      random_ltl)
    else convert_formula (Mparse.parse user_input)
  with _ ->
    print_string
      "Hmm, your input doesn't look right. Let's try again. Remember, you can \
       always see one of our example LTLs.\n";
    prompt_parse_convert ()

(* Usage *)
let rec cmdline_usage () =
  (******************************************)
  (* prompt, parse, conversion *)
  let user_input_ltl : Ltl.formula = prompt_parse_convert () in
  let string_from_ltl : string = Ltl.to_string user_input_ltl in
  let user_input_buchi : Buchi.automaton =
    Ltl2Buchi.convert_new user_input_ltl
  in
  let g = Visualizer.visualize user_input_buchi in
  let custom_png_name = "graph" in
  (******************************************)
  (* graphical settings *)
  let user_setting_initial_state : string = prompt_user_initial_state () in
  let setting_initial_state : bool =
    help_yn_to_bool user_setting_initial_state
  in
  let user_setting_label_ltl : string = prompt_user_label_ltl () in
  let setting_label_ltl : bool = help_yn_to_bool user_setting_label_ltl in
  let setting_label_custom : string = prompt_user_label_custom () in
  let user_setting_background_trans : string =
    prompt_user_background_trans ()
  in
  let setting_background_trans : bool =
    help_yn_to_bool user_setting_background_trans
  in

  let setting_bg_color : int =
    if setting_background_trans then 0
    else prompt_parse_user_background_color ()
  in
  let setting_shape_color : int = prompt_parse_user_shape_color () in
  let setting_font_color : int = prompt_parse_user_font_color () in
  (***********************************)
  (* print confirmations *)
  Printf.printf "\n";
  Printf.printf "LTL expression: %s\n" string_from_ltl;
  Printf.printf "The initial state %s marked with a separate box\n"
    (if setting_initial_state then "is" else "is not");
  Printf.printf
    "The automaton will %s labeled with the string representation of your LTL\n"
    (if setting_label_ltl then "be" else "not be");
  if setting_label_custom = "" then ()
  else Printf.printf "Customized label : %s\n" setting_label_custom;
  (*Printf.printf "Background transparency: %s\n" (if setting_background_trans
    then "yes" else "no"); if setting_background_trans then () else
    Printf.printf "Background color: %s\n" (if user_setting_bg_color = "" then
    "white" else user_setting_bg_color); Printf.printf "Font color: %s\n" (if
    user_setting_font_color = "" then "black" else user_setting_font_color);
    Printf.printf "Font color: %s\n" (if user_setting_shape_color = "" then
    "black" else user_setting_shape_color);*)
  (***********************************)
  (* output image *)
  Visualizer.output_png ~show_start:setting_initial_state
    ~ltl_label:(if setting_label_ltl then string_from_ltl else "")
    ~custom_label:setting_label_custom ~bg_transparent:setting_background_trans
    ~bg_color:setting_bg_color ~font_color:setting_font_color
    ~shape_color:setting_shape_color g user_input_buchi custom_png_name;
  open_png_file "graph.png";
  print_string
    "Do you want to create another LTL? Press y/n followed by Enter: ";
  let response = read_line () in
  match response with
  | "y" -> cmdline_usage ()
  | _ -> print_endline "bye"

(*final excecution*)
let () = cmdline_usage ()

(* dune exec bin/main.exe *)
