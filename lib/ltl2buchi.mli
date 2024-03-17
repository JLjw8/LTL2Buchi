open Ltl
open Buchi

(** LTL2BUCHI converts an LTL formula to Büchi automaton. The algorithm is based on the idea proposed by Kanso, B., & Kansou in their paper. Please refer to the reference in README for the exact citation*)
module type LTL2BUCHI = sig

    (** [to_string automaton] converts an automata to string*)
    val to_string : Buchi.automaton -> string
    
    (** [convert_new formula] converts an LTL formula to a Büchi automaton*)
    val convert_new: Ltl.formula -> Buchi.automaton

  end
 
  module Ltl2Buchi : LTL2BUCHI