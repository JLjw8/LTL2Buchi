(** BUCHI is the siganature of a Büchi automata module*)
module type BUCHI = sig
  open Ltl

  type state = int

  type transition = {
    from : state;
    to_ : state;
    condition : Ltl.formula;
  }

  type automaton = {
    states : state list;
    transitions : transition list;
    initial_states : state list;
    final_states : state list;
  }

  val create_state : int -> state
  (** [create_state i] creates a new state*)

  val create_transition : state -> state -> Ltl.formula -> transition
  (** [create_transition state1 state2] creates a transition from state1 to
      state2*)

  val create_self_transition :
    state -> state -> Ltl.formula -> bool -> transition
  (** [create_self_transition state1 state2] creates a self transition*)

  val get_final : automaton -> state list
  (** [get_final automaton] returns the list of final states*)

  val return_state_int : state -> int
  (** [return_state_int state] returns the number of the corresponding state*)

  val get_transition : automaton -> transition
  (** [get_transition automaton] returns the transition*)

  val get_condition : automaton -> Ltl.formula
  (** [get_condition automaton] returns the condition by accessing the
      transition*)

  val create_automaton : unit -> automaton
  (** [create_automaton ()] creates a new automaton *)

  val add_state : automaton -> state -> automaton
  (** [add_state automaton state] adds a state to the automaton *)

  val add_transition : automaton -> transition -> automaton
  (** [add_transitio automaton transition] adds a transition to the automaton *)

  val set_initial_states : automaton -> state list -> automaton
  (** [set_initial_states automaton states] define initial states of the
      automaton *)

  val set_final_states : automaton -> automaton
  (** [set_final_states automaton] get the final states of the automaton *)
end

open Ltl

module Buchi : BUCHI = struct
  type state = int

  type transition = {
    from : state;
    to_ : state;
    condition : Ltl.formula;
  }

  type automaton = {
    states : state list;
    transitions : transition list;
    initial_states : state list;
    final_states : state list;
  }

  let create_state (i : int) : state = i
  let create_transition s1 s2 cond = { from = s1; to_ = s2; condition = cond }

  let create_self_transition s1 s2 cond id =
    if id then { from = s1; to_ = s2; condition = cond }
    else { from = s1; to_ = s2; condition = Any 1 }

  let get_final automaton =
    match List.rev automaton.states with
    | [] -> failwith "Automaton has no states."
    | final_state :: _ -> [ final_state ]

  let return_state_int state = state

  let get_transition automaton =
    match automaton.transitions with
    | [] -> failwith "Automaton has no transitions."
    | transition :: _ -> transition

  let get_condition automaton =
    let transition = get_transition automaton in
    transition.condition

  let create_automaton () =
    {
      states = [];
      transitions = [];
      initial_states = [];
      final_states = [] (* Initialized as an empty list *);
    }

  let add_state automaton state =
    { automaton with states = automaton.states @ [ state ] }

  let add_transition automaton transition =
    { automaton with transitions = transition :: automaton.transitions }

  let set_initial_states automaton initial_states =
    { automaton with initial_states }

  let set_final_states automaton =
    { automaton with final_states = get_final automaton }
end
