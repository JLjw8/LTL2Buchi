module type LTL = sig
  
  (** logic_bin is the logical binary operator. It includes And(&&), Or(||), Implies(->), and Eqv(=)*)
  type logic_bin =
    | And
    | Or
    | Implies
    | Eqv

  (** logic_un is the logical unary operator. It includes Neg(¬).*)
  type logic_un = Neg

  (** temporal_bin is the temporal binary operator. It includes Until(U) and Release(R)*)
  type temporal_bin =
    | Until
    | Release

  (** temporal_un is the temporal unary operator. It includes *)
  type temporal_un =
    | Finally

  (** Formula includes a formal definition of an LTL formula type. Requires: Any can only be 1. It refers to "any input is true"*)
  type formula =
    | Any of int
    | Bool of bool
    | AP of string
    | LOP_bin of logic_bin * formula ref * formula ref
    | LOP_un of logic_un * formula ref
    | TOP_bin of formula ref * temporal_bin * formula ref
    | TOP_un of temporal_un * formula ref

  (** [is_propositional f] returns true if f is a propositional formula*)
  val is_propositional: formula -> bool
  
  (** [compare phi psi] returns true if phi is structurally equal to psi*)
  val compare: formula -> formula -> bool

  (** [to_string phi] converts an LTL type into string*)
  val to_string: formula -> string

  (** [neg phi] returns the negated phi*)
  val neg: formula -> formula

  (** [simplify_neg f] returns a formula that removes the outer Neg*)
  val simplify_neg : formula -> formula

  (** [finally phi] returns an updated LTL type with "finally" proceeded. Phi eventually has to hold (somewhere on the subsequent path).*)
  val finally: formula -> formula

  (** [convert_finally f] converts a Finally expression to an Until expression*)
  val convert_finally: formula -> formula

  (** [convert_implies f] converts an Implies expression to a propositional expression*)
  val convert_implies: formula -> formula

  (** [until phi psi] returns an updated LTL type with "until" proceeded. Psi has to hold at least until phi becomes true, which must hold at the current or a future position.*)
  val until: formula -> formula -> formula

  (** [conjunction phi psi] returns an updated LTL type with "and" proceeded. *)
  val conjunction: formula -> formula -> formula

  (** [disjunction phi psi] returns an updated LTL type with "or" proceeded*)
  val disjunction: formula -> formula -> formula

  (** [release phi psi] returns an updated LTL type with "release" proceeded. Phi has to be true until and including the point where psi first becomes true; if phi never becomes true, psi must remain true forever.*)
  val release: formula -> formula -> formula

  (** [implies phi psi] returns an updated LTL type with "implies" proceeded*)
  val implies: formula -> formula -> formula

  (** [expand f] returns an expanded formula, where the output is either a proposition or an Until expression*)
  val expand: formula -> formula

  (** [extract_propositions] extracts all propositonal formulas and return them as a list*)
  val extract_propositions : formula -> formula list

  (** [continuous_conjunction formula list i j] creates a massive conjunction term with heuristic*)
  val continuous_conjunction: formula list -> int -> int -> formula
end

module Ltl : LTL