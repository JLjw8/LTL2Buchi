open OUnit2
open Ltl2ba
open Ltl
open Ltl2buchi
open Visualizer
open Buchi
open Ltlparsing

(** TEST PLAN *)
(* Tested with OUnit : 
   - Parsing string representation into LTL ast (parsing/ast.ml lexer.mll 
    mparse.ml parser.mly) with glass box testing. We ensured every word in lexer
    and every rule in parser is tested.  
   - Converting LTL ast to Buchi automata (lib/ltl.ml buchi.ml ltl2buchi.ml) 
    with primarlily glass box and some black box testing. We ensured every path
    in the conversion algorithm is tested.
   
   Tested manually : 
   - Converting Buchi automata to ocamlgraph and creating image (lib/visualizer.ml)
    with glass box testing

   - Command-line interface (bin/main.ml) with glass box testing
   
   
   We believe we did not omit anything in
   testing. *)

(*******************************************************************************
  Helper functions for tests
  *******************************************************************************)
let run_and_report test_name test_logic =
  try
    test_logic ();
    Printf.printf "%s passed.\n" test_name
  with Assert_failure (file, line, col) ->
    let msg =
      Printf.sprintf "File \"%s\", line %d, characters %d: Assertion failed"
        file line col
    in
    Printf.printf "%s FAILED: %s\n" test_name msg;
    raise (Assert_failure (file, line, col))

(*******************************************************************************
  Tests for LTL
  *******************************************************************************)
module Test_LTL = Ltl

let ltl1 = Test_LTL.Bool true
let ltl2 = Test_LTL.AP "phi"
let ltl3 = Test_LTL.AP "psi"
let ltl4 = Test_LTL.LOP_bin (And, ref ltl2, ref ltl3)
let ltl5 = Test_LTL.LOP_bin (Implies, ref ltl2, ref ltl3)
let ltl6 = Test_LTL.LOP_un (Neg, ref ltl5)
let ltl7 = Test_LTL.TOP_bin (ref ltl4, Until, ref ltl5)
let ltl9 = Test_LTL.TOP_un (Finally, ref ltl5)
let ltl10 = Test_LTL.LOP_un (Neg, ref ltl4)
let ltl11 = Test_LTL.TOP_bin (ref ltl2, Until, ref ltl3)
let ltl12 = Test_LTL.TOP_bin (ref (Test_LTL.AP "eps"), Until, ref ltl11)

let to_string_test expected_output input _ =
  let test_name = "to_string_test" in
  (* Extract the test name from the context *)
  let test_logic () =
    let actual_output = Test_LTL.to_string input in
    assert_equal ~printer:(fun x -> x) expected_output actual_output
  in
  run_and_report test_name test_logic

let neg_test (expected_output : Ltl.formula) (input : Ltl.formula) _test_ctxt =
  let test_name = "neg_test" in
  (* Static test name, could be derived differently *)
  let test_logic () =
    let actual_output = Test_LTL.neg input in
    (* Comparing the actual output with the expected output *)
    assert_equal ~printer:Test_LTL.to_string expected_output actual_output
  in
  run_and_report test_name test_logic

let expand_test expected_output (input : Ltl.formula) _ =
  let test_name = "expand_test" in
  let test_logic () =
    let actual_output = Test_LTL.expand input in
    (* Comparing the actual output with the expected output *)
    assert_equal ~printer:Test_LTL.to_string expected_output actual_output
  in
  run_and_report test_name test_logic

let rec to_str_tempt fl =
  match fl with
  | [] -> "]"
  | h :: t ->
      if t = [] then Test_LTL.to_string h ^ to_str_tempt []
      else Test_LTL.to_string h ^ "; " ^ to_str_tempt t

let extract_proposition_test expected_output (input : Ltl.formula) _ =
  let test_name = "extract_proposition_test" in
  let test_logic () =
    let actual_output = Test_LTL.extract_propositions input in
    let actual_output_str = "[" ^ to_str_tempt actual_output in
    assert_equal ~printer:(fun x -> x) expected_output actual_output_str
  in
  run_and_report test_name test_logic

let ltl_tests =
  let open Test_LTL in
  [
    "Ltl.to_string test 1: Bool" >:: to_string_test "true" ltl1;
    "Ltl.to_string test 2: And" >:: to_string_test "phi ∧ psi" ltl4;
    "Ltl.to_string test 3: Implies" >:: to_string_test "phi => psi" ltl5;
    "Ltl.to_string test 4: Neg" >:: to_string_test "¬ (phi => psi)" ltl6;
    "Ltl.to_string test 5: Until"
    >:: to_string_test "(phi ∧ psi) U (phi => psi)" ltl7;
    "Ltl.neg test 1: Bool" >:: neg_test (Bool false) ltl1;
    "Ltl.neg test 2: Neg" >:: neg_test ltl5 ltl6;
    "Ltl.neg test 3: LOP_bin Or"
    >:: neg_test
          (LOP_bin
             (Or, ref (LOP_un (Neg, ref ltl2)), ref (LOP_un (Neg, ref ltl3))))
          ltl4;
    "Ltl.neg test 4: LOP_bin Implies"
    >:: neg_test
          (LOP_bin
             (Or, ref (LOP_un (Neg, ref ltl2)), ref (LOP_un (Neg, ref ltl3))))
          ltl4;
    "Ltl.expand test 1: Bool" >:: expand_test ltl1 ltl1;
    "Ltl.expand test 2: AP" >:: expand_test ltl2 ltl2;
    "Ltl.expand test 3: AND" >:: expand_test ltl4 ltl4;
    "Ltl.expand test 4: Implies"
    >:: expand_test (LOP_bin (Or, ref (neg ltl2), ref ltl3)) ltl5;
    "Ltl.expand test 5: Neg"
    >:: expand_test
          (LOP_un (Neg, ref (LOP_bin (Or, ref (neg ltl2), ref ltl3))))
          ltl6;
    "Ltl.expand test 6: Until"
    >:: expand_test
          (TOP_bin
             ( ref (LOP_bin (And, ref ltl2, ref ltl3)),
               Until,
               ref (LOP_bin (Or, ref (neg ltl2), ref ltl3)) ))
          ltl7;
    "Ltl.expand test 7: Finally"
    >:: expand_test
          (TOP_bin
             ( ref (Bool true),
               Until,
               ref (LOP_bin (Or, ref (neg ltl2), ref ltl3)) ))
          ltl9;
    "Extract proposition test 1: Single AP"
    >:: extract_proposition_test "[phi]" ltl2;
    "Extract proposition test 2: AND operation"
    >:: extract_proposition_test "[phi ∧ psi]" ltl4;
    "Extract proposition test 3: Implies operation"
    >:: extract_proposition_test "[¬ phi v psi]" ltl5;
    "Extract proposition test 4: Negation"
    >:: extract_proposition_test "[¬ (¬ phi v psi)]" ltl6;
    "Extract proposition test 5: Until operation"
    >:: extract_proposition_test "[phi ∧ psi; ¬ phi v psi]" ltl7;
    "Extract proposition test 6: Finally operation"
    >:: extract_proposition_test "[true; ¬ phi v psi]" ltl9;
    "Extract proposition test 7: Negation of AND"
    >:: extract_proposition_test "[¬ (phi ∧ psi)]" ltl10;
  ]

(*******************************************************************************
  Tests for LTL2BUCHI
  *******************************************************************************)

module Test_ltl2buchi = Ltl2Buchi

let convert_new_test expected_output (input : Ltl.formula) _ =
  let test_name = "expand_test" in
  let test_logic () =
    let actual_output = Test_ltl2buchi.convert_new input in
    let actual_str = Test_ltl2buchi.to_string actual_output in
    assert_equal ~printer:(fun x -> x) expected_output actual_str
  in
  run_and_report test_name test_logic

let ltl2buchi_tests =
  [
    "Test_ltl2buchi.convert_new Test 1: Proposition Bool:"
    >:: convert_new_test
          "States: {0, 1}\n\
           Transitions: {0 -[true]-> 1, 1 -[1]-> 1}\n\
           Initial States: {0}\n\
           Final States: {1}" ltl1;
    "Test_ltl2buchi.convert_new Test 2: Proposition AP:"
    >:: convert_new_test
          "States: {0, 1}\n\
           Transitions: {0 -[phi]-> 1, 1 -[1]-> 1}\n\
           Initial States: {0}\n\
           Final States: {1}" ltl2;
    "Test_ltl2buchi.convert_new Test 3: Proposition And:"
    >:: convert_new_test
          "States: {0, 1}\n\
           Transitions: {0 -[phi ∧ psi]-> 1, 1 -[1]-> 1}\n\
           Initial States: {0}\n\
           Final States: {1}" ltl4;
    "Test_ltl2buchi.convert_new Test 4: Proposition Neg:"
    >:: convert_new_test
          "States: {0, 1}\n\
           Transitions: {0 -[¬ (phi ∧ psi)]-> 1, 1 -[1]-> 1}\n\
           Initial States: {0}\n\
           Final States: {1}" ltl10;
    "Test_ltl2buchi.convert_new Test 4: Non-Proposition Implies:"
    >:: convert_new_test
          "States: {0, 1}\n\
           Transitions: {0 -[¬ phi v psi]-> 1, 1 -[1]-> 1}\n\
           Initial States: {0}\n\
           Final States: {1}" ltl5;
    "Test_ltl2buchi.convert_new Test 5: Simple until:"
    >:: convert_new_test
          "States: {0, 1}\n\
           Transitions: {1 -[1]-> 1, 0 -[psi]-> 1, 0 -[phi ∧ ¬ psi]-> 0}\n\
           Initial States: {0}\n\
           Final States: {1}" ltl11;
    "Test_ltl2buchi.convert_new Test 6: Complexed until:"
    >:: convert_new_test
          "States: {0, 1, 2}\n\
           Transitions: {2 -[1]-> 2, 1 -[psi]-> 2, 1 -[phi ∧ ¬ psi]-> 1, 0 \
           -[psi]-> 2, 0 -[eps ∧ ¬ phi]-> 1, 0 -[eps ∧ ¬ psi]-> 0}\n\
           Initial States: {0}\n\
           Final States: {2}" ltl12;
  ]

(*******************************************************************************
  Tests for Visualizer
  *******************************************************************************)
let state_list1 = [ 0; 1; 2 ]
let final_state_list1 = [ 0; 2 ]

let trans_list1 : Buchi.transition list =
  [
    { from = 0; to_ = 2; condition = AP "01" };
    { from = 1; to_ = 0; condition = AP "01" };
    { from = 0; to_ = 1; condition = AP "01" };
  ]

let aa1 : Buchi.automaton =
  {
    states = state_list1;
    transitions = trans_list1;
    initial_states = [ 0; 1 ];
    final_states = final_state_list1;
  }

let aa : Buchi.automaton = Test_ltl2buchi.convert_new ltl12

let () =
  Visualizer.output_png ~show_start:true (Visualizer.visualize aa) aa "vltl1"

let integrated1 = Visualizer.visualize aa1
let () = Visualizer.output_png (*~show_start:true*) integrated1 aa1 "vinte1"

(*******************************************************************************
  Tests for Parsing
  *******************************************************************************)

let make_parse n res s = n >:: fun _ -> assert_equal res (Mparse.parse_show s)

let parsing_tests =
  [
    make_parse "AP boolean 1" "pbool*true" "true";
    make_parse "AP boolean 2" "pbool*true" "TRUE";
    make_parse "AP boolean 3" "pbool*false" "False";
    make_parse "AP text 1" "pstr*testing" "testing";
    make_parse "AP text 2" "pstr*TesTing" "TesTing";
    make_parse "AP text 3" "pstr*testing" "testing is true";
    make_parse "AP num 1" "pstr*22" "22";
    make_parse "AP numtext 1" "pstr*may5day" "may5day";
    make_parse "AP paran" "pstr*loveStone" "(loveStone)";
    make_parse "AP numtext 2" "pstr*SometHing12345" "SometHing12345";
    make_parse "LOP AND 1" "(pstr*p1 plb*AND pstr*p2)" "p1 and p2";
    make_parse "LOP AND 2" "(pstr*abc plb*AND pstr*def)" "abc & def";
    make_parse "LOP AND 3" "(pstr*12 plb*AND pstr*34)" "12 AND 34";
    make_parse "LOP AND 4" "(pbool*true plb*AND pstr*M2d)" "True And M2d";
    make_parse "LOP OR 1" "(pstr*P1 plb*OR pstr*22)" "P1 OR 22";
    make_parse "LOP OR 2" "(pstr*Sth plb*OR pstr*ham)" "Sth or ham";
    make_parse "LOP OR 3" "(pbool*true plb*OR pbool*false)" "true Or false";
    make_parse "LOP OR 4" "(pstr*33 plb*OR pstr*22)" "33 | 22";
    make_parse "LOP Implies 1" "(pstr*Mdd plb*IMPLIES pbool*true)" "Mdd => true";
    make_parse "LOP Implies 2" "(pstr*mayday plb*IMPLIES pstr*1997)"
      "mayday implies 1997";
    make_parse "LOP Implies 3" "(pstr*arron plb*IMPLIES pstr*BURR)"
      "arron IMPLIES BURR";
    make_parse "LOP Implies 4" "(pstr*one plb*IMPLIES pstr*2)" "one Implies 2";
    make_parse "LOP Eqv 1" "(pstr*STONE plb*EQUAL pstr*48)"
      "STONE equivalent 48";
    make_parse "LOP Eqv 2" "(pstr*fourtyeight plb*EQUAL pstr*48)"
      "fourtyeight = 48";
    make_parse "LOP Eqv 3" "(pstr*STONE plb*EQUAL pbool*true)"
      "STONE equals true";
    make_parse "LOP Eqv 4" "(pstr*something plb*EQUAL pstr*hate)"
      "something Equals hate";
    make_parse "LOP Eqv 5" "(pbool*false plb*EQUAL pbool*true)"
      "FALSE EQUALS true";
    make_parse "LOP Neg 1" "plu*NOT pstr*way" "NO way";
    make_parse "LOP Neg 2" "plu*NOT pbool*true" "NOT true";
    make_parse "LOP Neg 3" "plu*NOT pbool*false" "not False";
    make_parse "LOP Neg 4" "plu*NOT pstr*WAY" "! WAY";
    make_parse "LOP Neg 5" "plu*NOT pstr*Way" "no Way";
    make_parse "LOP Neg 6" "plu*NOT pstr*213" "No 213";
    make_parse "LOP Neg 7" "plu*NOT pstr*P1" "Not P1";
    make_parse "TOP until 1" "(pstr*sunny ptb*UNTIL pstr*Rain)"
      "sunny until Rain";
    make_parse "TOP Until 2" "(pstr*12345 ptb*UNTIL pstr*sun)" "12345 Until sun";
    make_parse "TOP Until 3" "(pbool*true ptb*UNTIL pstr*SUN)" "true UNTIL SUN";
    make_parse "TOP Until 4" "(pstr*123456 ptb*UNTIL pstr*PHTTabel)"
      "123456 Til PHTTabel";
    make_parse "TOP Until 5" "(pbool*false ptb*UNTIL pstr*nologic)"
      "False til nologic";
    make_parse "TOP RELEASE 1" "(pstr*Sun ptb*RELEASE pstr*O2)" "Sun Release O2";
    make_parse "TOP RELEASE 2" "(pstr*oxygen ptb*RELEASE pstr*123)"
      "oxygen Release 123";
    make_parse "TOP RELEASE 3" "(pstr*robot ptb*RELEASE pbool*true)"
      "robot Release true";
    make_parse "TOP WeakU 1" "(pstr*Me ptb*WEAKU pstr*DINNER)"
      "Me Weak_until DINNER";
    make_parse "TOP WeakU 2" "(pstr*Ham ptb*WEAKU pbool*false)"
      "Ham weak_until False";
    make_parse "TOP Finally 1" "ptu*FINALLY pstr*done3110" "FINALLY done3110";
    make_parse "TOP Finally 2" "ptu*FINALLY pstr*3110" "Finally 3110";
    make_parse "TOP Finally 3" "ptu*FINALLY pstr*DONE" "finally DONE";
    make_parse "TOP Finally 4" "ptu*FINALLY pbool*true" "final true";
    make_parse "TOP Finally 5" "ptu*FINALLY pbool*false" "Final FALSE";
    make_parse "TOP Always 1" "ptu*ALWAYS pstr*happy" "ALWAYS happy";
    make_parse "TOP Always 2" "ptu*ALWAYS pstr*Happy" "ALWAYS Happy";
    make_parse "TOP Always 3" "ptu*ALWAYS pbool*true" "ALWAYS TRUE";
    make_parse "TOP Always 4" "ptu*ALWAYS pbool*false" "ALWAYS false";
    make_parse "TOP Always 5" "ptu*ALWAYS pstr*5525" "ALWAYS 5525";
    make_parse "LOP LOP" "plu*NOT (pstr*p1 plb*OR pstr*p2)" "NO (p1 Or p2)";
    make_parse "TOP LOP"
      "(plu*NOT pstr*raining ptb*UNTIL (pstr*temp plb*EQUAL pstr*30))"
      "(not raining) until (temp=30)";
    make_parse "LOP TOP"
      "(ptu*ALWAYS pstr*healthy plb*IMPLIES ptu*FINALLY pstr*goodEnd)"
      "(Always healthy) => (Finally goodEnd)";
    make_parse "TOP TOP" "ptu*FINALLY (pstr*hunger ptb*RELEASE pstr*anger)"
      "finally hunger release anger";
    make_parse "Final parse test"
      "(ptu*ALWAYS pstr*HUNGRY ptb*UNTIL (pstr*512 plb*OR (pstr*lunch \
       plb*EQUAL pbool*true)))"
      "(always HUNGRY) until (512 OR (lunch = TRUE))";
  ]

let suite =
  "test suite for ltl2buchi converter"
  >::: List.flatten [ ltl_tests; ltl2buchi_tests; parsing_tests ]

let () = run_test_tt_main suite
