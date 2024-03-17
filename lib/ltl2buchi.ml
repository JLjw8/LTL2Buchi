open Ltl
open Buchi

module type LTL2BUCHI = sig
  val to_string : Buchi.automaton -> string
  (** [to_string automaton] converts an automata to string*)

  val convert_new : Ltl.formula -> Buchi.automaton
  (** [convert_new formula] converts an LTL formula to Buchi Automata*)
end

module Ltl2Buchi : LTL2BUCHI = struct
  open Ltl
  open Buchi

  let build_automaton f =
    let lst = Ltl.extract_propositions f in
    let length = List.length lst in
    let automaton = Buchi.create_automaton () in

    (* Add states *)
    let automaton_with_states =
      List.fold_left Buchi.add_state automaton
        (List.init length Buchi.create_state)
    in

    (* Add transitions *)
    let automaton_with_transitions =
      let add_transitions acc i j =
        let condition =
          if i = j then
            Ltl.conjunction (List.nth lst i)
              (Ltl.neg (List.nth lst (length - 1)))
          else if j = length - 1 then List.nth lst j
          else Ltl.continuous_conjunction lst i j
        in
        let transition =
          if i = j && i = length - 1 then
            Buchi.create_self_transition (Buchi.create_state i)
              (Buchi.create_state j) condition false
          else
            Buchi.create_transition (Buchi.create_state i)
              (Buchi.create_state j) condition
        in
        Buchi.add_transition acc transition
      in
      let rec add_all_transitions acc i j =
        if i >= length then acc
        else if j >= length then add_all_transitions acc (i + 1) (i + 1)
        else add_all_transitions (add_transitions acc i j) i (j + 1)
      in
      add_all_transitions automaton_with_states 0 0
    in

    (* Set initial and final states *)
    let automaton_with_initial_states =
      Buchi.set_initial_states automaton_with_transitions
        [ Buchi.create_state 0 ]
    in
    let final_automaton =
      Buchi.set_final_states automaton_with_initial_states
    in

    Buchi.set_final_states final_automaton

  let convert_new formula =
    let f = expand formula in
    (* if the input is a propositional formula *)
    match is_propositional f with
    | true ->
        let s0 = Buchi.create_state 0 in
        let s1 = Buchi.create_state 1 in
        let transition1 = Buchi.create_transition s0 s1 f in
        let transition2 = Buchi.create_self_transition s1 s1 f false in
        let automaton =
          {
            states = [ s0; s1 ];
            transitions = [ transition1; transition2 ];
            initial_states = [ s0 ];
            final_states = [ s1 ];
          }
        in
        automaton
    | false -> build_automaton formula (* if the input is a temporal formula *)

  let to_string automaton : string =
    let states_str =
      String.concat ", " (List.map string_of_int automaton.states)
    in
    let transitions_str =
      let transition_to_string t =
        Printf.sprintf "%d -[%s]-> %d" t.Buchi.from
          (Ltl.to_string t.Buchi.condition)
          t.Buchi.to_
      in
      String.concat ", " (List.map transition_to_string automaton.transitions)
    in
    let initial_states_str =
      String.concat ", " (List.map string_of_int automaton.initial_states)
    in
    let final_state_str =
      String.concat ", " (List.map string_of_int automaton.final_states)
    in

    Printf.sprintf
      "States: {%s}\n\
       Transitions: {%s}\n\
       Initial States: {%s}\n\
       Final States: {%s}" states_str transitions_str initial_states_str
      final_state_str
end
