(** BUCHI includes the signature of Büchi automaton module. It defines the basic component of a Büchi automaton and needed functions to construct a Büchi automaton*)
module type BUCHI = sig
  open Ltl

  (** state refers to a state in the buchi automata. It is represented with natural number.*)
  type state = int

  (** transition refers to the transition from one state to the other state in a buchi automata.*)
  type transition = {
    from : state;
    to_ : state;
    condition : Ltl.formula;
  }

  (** automaton refers to a buchi automata. It has four fields that record states, transitions, initial states, and final states*)
  type automaton = {
    states : state list;
    transitions : transition list;
    initial_states : state list;
    final_states : state list;
  }

  (** [create_state i] creates a new state*)
  val create_state: int -> state

  (** [create_transition state1 state2] creates a transition from state1 to state2*)
  val create_transition: state -> state -> Ltl.formula -> transition

  (** [create_self_transition state1 state2] creates a self transition*)
  val create_self_transition: state -> state -> Ltl.formula -> bool -> transition

  (** [get_final automaton] returns the list of final states*)
  val get_final: automaton -> state list

  (** [return_state_int state] returns the number of the corresponding state*)
  val return_state_int: state -> int

  (** [get_transition automaton] returns the transition*)
  val get_transition:automaton -> transition

  (** [get_condition automaton] returns the condition by accessing the transition*)
  val get_condition:automaton -> Ltl.formula

  (** [create_automaton ()] creates a new automaton *)
  val create_automaton : unit -> automaton

  (** [add_state automaton state] adds a state to the automaton *)
  val add_state : automaton -> state -> automaton

  (** [add_transitio automaton transition] adds a transition to the automaton *)
  val add_transition : automaton -> transition -> automaton

  (** [set_initial_states automaton states] define initial states of the automaton *)
  val set_initial_states : automaton -> state list -> automaton

  (** [set_final_states automaton] get the final states of the automaton *)
  val set_final_states : automaton -> automaton

  end
  
module Buchi:BUCHI