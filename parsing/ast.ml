(** The abstract syntax tree type. *)

type logic_bin =
  | And
  | Or
  | Implies
  | Eqv

type logic_un = Neg

type temporal_bin =
  | Until
  | WeakU
  | Release

type temporal_un =
  | Finally
  | Always

type formula =
  | Bool of bool
  | AP of string
  | LOP_bin of logic_bin * formula ref * formula ref
  | LOP_un of logic_un * formula ref
  | TOP_bin of formula ref * temporal_bin * formula ref
  | TOP_un of temporal_un * formula ref
