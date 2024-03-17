module type LTL = sig
  type logic_bin =
    | And
    | Or
    | Implies
    | Eqv

  type logic_un = Neg

  type temporal_bin =
    | Until
    | Release

  type temporal_un = Finally

  type formula =
    | Any of int
    | Bool of bool
    | AP of string
    | LOP_bin of logic_bin * formula ref * formula ref
    | LOP_un of logic_un * formula ref
    | TOP_bin of formula ref * temporal_bin * formula ref
    | TOP_un of temporal_un * formula ref

  val is_propositional : formula -> bool
  (** [is_propositional f] returns true if f is a propositional formula*)

  val compare : formula -> formula -> bool
  (** [compare phi psi] returns true if phi is structurally equal to psi*)

  val to_string : formula -> string
  (** [to_string phi] converts an LTL type into string*)

  val neg : formula -> formula
  (** [neg phi] returns the negated phi*)

  val simplify_neg : formula -> formula
  (** [simplify_neg f] returns a formula that removes the outer Neg*)

  val finally : formula -> formula
  (** [finally phi] returns an updated LTL type with "finally" proceeded. Phi
      eventually has to hold (somewhere on the subsequent path).*)

  val convert_finally : formula -> formula
  (** [convert_finally f] converts a Finally expression to an Until expression*)

  val convert_implies : formula -> formula
  (** [convert_implies f] converts an Implies expression to an Until expression*)

  val until : formula -> formula -> formula
  (** [until psi] returns an updated LTL type with "until" proceeded. Psi has to
      hold at least until phi becomes true, which must hold at the current or a
      future position.*)

  val conjunction : formula -> formula -> formula
  (** [conjunction phi psi] returns an updated LTL type with "and" proceeded. *)

  val disjunction : formula -> formula -> formula
  (** [disjunction phi psi] returns an updated LTL type with "or" proceeded*)

  val release : formula -> formula -> formula
  (** [release phi psi] returns an updated LTL type with "release" proceeded.
      Phi has to be true until and including the point where psi first becomes
      true; if phi never becomes true, psi must remain true forever.*)

  val implies : formula -> formula -> formula
  (** [implies phi psi] returns an updated LTL type with "implies" proceeded*)

  val expand : formula -> formula
  (** [expand f] returns an expanded formula, where the output is either a
      proposition or an Until expression*)

  val extract_propositions : formula -> formula list
  (** [extract_propositions] extracts all propositonal formulas and return them
      as a list*)

  val continuous_conjunction : formula list -> int -> int -> formula
  (** [continuous_conjunction formula list i j] creates a massive conjunction
      term with heuristic*)
end

(* Define a module Ltl that satisfies the signature LTL *)
module Ltl : LTL = struct
  type logic_bin =
    | And
    | Or
    | Implies
    | Eqv

  type logic_un = Neg

  type temporal_bin =
    | Until
    | Release

  type temporal_un = Finally

  type formula =
    | Any of int
    | Bool of bool
    | AP of string
    | LOP_bin of logic_bin * formula ref * formula ref
    | LOP_un of logic_un * formula ref
    | TOP_bin of formula ref * temporal_bin * formula ref
    | TOP_un of temporal_un * formula ref

  let rec is_propositional f =
    match f with
    | Bool _ -> true
    | AP _ -> true
    | LOP_un (_, f) -> is_propositional !f
    | LOP_bin (_, f1, f2) -> is_propositional !f1 && is_propositional !f2
    | TOP_bin _ -> false
    | TOP_un _ -> false
    | _ -> failwith "Not Applicable"

  let rec compare phi psi =
    match (phi, psi) with
    | Bool b1, Bool b2 -> b1 = b2
    | AP str1, AP str2 -> str1 = str2
    | LOP_bin (logic1, f1_1, f1_2), LOP_bin (logic2, f2_1, f2_2) ->
        logic1 = logic2 && compare !f1_1 !f2_1 && compare !f1_2 !f2_2
    | LOP_un (l1, f1), LOP_un (l2, f2) -> l1 = l2 && compare !f1 !f2
    | TOP_un (t1, f1), TOP_un (t2, f2) -> t1 = t2 && compare !f1 !f2
    | TOP_bin (f1_1, temporal1, f1_2), TOP_bin (f2_1, temporal2, f2_2) ->
        temporal1 = temporal2 && compare !f1_1 !f2_1 && compare !f1_2 !f2_2
    | _ -> false

  let rec to_string_helper phi =
    let to_paren s = "(" ^ s ^ ")" in
    match phi with
    | Bool b -> string_of_bool b
    | AP s -> s
    | LOP_bin (logic_bin, f1, f2) ->
        let logic_bin_str =
          match logic_bin with
          | And -> "∧"
          | Or -> "v"
          | Implies -> "=>"
          | Eqv -> "<=>"
        in
        to_paren
          (to_string_helper !f1 ^ " " ^ logic_bin_str ^ " "
         ^ to_string_helper !f2)
    | LOP_un (logic_un, f) ->
        let logic_un_str =
          match logic_un with
          | Neg -> "¬"
        in
        logic_un_str ^ " " ^ to_string_helper !f
    | TOP_bin (f1, temporal_bin, f2) ->
        let temporal_bin_str =
          match temporal_bin with
          | Until -> "U"
          | Release -> "R"
        in
        to_paren
          (to_string_helper !f1 ^ " " ^ temporal_bin_str ^ " "
         ^ to_string_helper !f2)
    | TOP_un (temporal_un, f) ->
        let temporal_un_str =
          match temporal_un with
          | Finally -> "F"
        in
        temporal_un_str ^ " " ^ to_string_helper !f
    | Any _ -> "1"

  let to_string phi =
    let s = to_string_helper phi in
    if s.[0] = '(' && s.[String.length s - 1] = ')' then
      String.sub s 1 (String.length s - 2)
    else s

  let rec neg phi =
    match phi with
    | Bool b -> Bool (not b)
    | LOP_un (Neg, f) -> !f (* Removing double negation *)
    | LOP_bin (And, f1, f2) ->
        LOP_bin (Or, ref (neg !f1), ref (neg !f2)) (* De Morgan's Law *)
    | LOP_bin (Or, f1, f2) ->
        LOP_bin (And, ref (neg !f1), ref (neg !f2)) (* De Morgan's Law *)
    | LOP_bin (Implies, f1, f2) ->
        LOP_bin (Or, ref (neg !f1), ref !f2)
        (* Implication in terms of And and Neg *)
    | LOP_bin (Eqv, f1, f2) ->
        let neg_f1_f2 = LOP_bin (And, ref (neg !f1), ref (neg !f2)) in
        let f1_f2 = LOP_bin (And, ref !f1, ref !f2) in
        LOP_bin (Or, ref neg_f1_f2, ref f1_f2)
        (* Equivalence in terms of And, Or, and Neg *)
    | AP _ -> LOP_un (Neg, ref phi) (* Directly negate atomic propositions *)
    | TOP_bin (_, _, _) | TOP_un (_, _) -> phi
    | _ -> failwith "Not Applicable"

  let simplify_neg f =
    match f with
    | LOP_un (Neg, f') -> !f'
    | _ -> f

  let finally phi = TOP_un (Finally, ref phi)
  let until phi psi = TOP_bin (ref phi, Until, ref psi)

  let convert_finally f =
    match f with
    | TOP_un (Finally, f') -> until (Bool true) !f'
    | _ -> failwith "Type Error"

  let conjunction phi psi = LOP_bin (And, ref phi, ref psi)
  let disjunction phi psi = LOP_bin (Or, ref phi, ref psi)
  let implies phi psi = LOP_bin (Implies, ref phi, ref psi)

  let convert_implies f =
    match f with
    | LOP_bin (Implies, f1, f2) -> disjunction (neg !f1) !f2
    | _ -> failwith "Type Error"

  let convert_eqv f =
    match f with
    | LOP_bin (Eqv, f1, f2) ->
        let f1' = convert_implies (LOP_bin (Implies, f1, f2)) in
        let f2' = convert_implies (LOP_bin (Implies, f2, f1)) in
        conjunction f1' f2'
    | _ -> failwith "Type Error"

  let release phi psi = TOP_bin (ref phi, Release, ref psi)

  let convert_release f =
    match f with
    | TOP_bin (f1, Release, f2) -> until !f1 !f2
    | _ -> failwith "Type Error"

  let rec expand f =
    match f with
    | Bool _ | AP _ -> f
    | LOP_bin (lop_bin, f1, f2) -> begin
        match lop_bin with
        | Implies ->
            let f' = convert_implies f in
            expand f'
        | Eqv ->
            let f' = convert_eqv f in
            expand f'
        | And | Or -> LOP_bin (lop_bin, ref (expand !f1), ref (expand !f2))
      end
    | LOP_un (Neg, f') -> LOP_un (Neg, ref (expand !f'))
    | TOP_bin (f1, temporal_bin, f2) -> (
        match temporal_bin with
        | Until -> TOP_bin (ref (expand !f1), Until, ref (expand !f2))
        | Release ->
            let f' = convert_release f in
            expand f')
    | TOP_un (Finally, _) ->
        let f' = convert_finally f in
        expand f'
    | _ -> failwith "Not Applicable"

  let rec extract_prop f acc =
    let f' = expand f in
    match is_propositional f' with
    | true ->
        if not (List.exists (fun x -> compare f x) acc) then f' :: acc else acc
    | false -> (
        match f' with
        | LOP_bin (_, f1, f2) | TOP_bin (f1, _, f2) ->
            extract_prop !f1 (extract_prop !f2 acc)
        | LOP_un (_, f) | TOP_un (_, f) -> extract_prop !f acc
        | _ -> acc)

  let extract_propositions f = extract_prop f []

  let continuous_conjunction lst i j =
    let rec aux acc k =
      if k > j then acc
      else
        let f =
          if k = i || k = j then neg (List.nth lst k) else List.nth lst k
        in
        aux (conjunction acc f) (k + 1)
    in
    aux (List.nth lst i) (i + 1)
end
