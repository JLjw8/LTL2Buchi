open Graph

module type VisualizerType = sig
  open Buchi

  type graph

  val visualize : Buchi.automaton -> graph

  val output_png :
    ?show_start:bool ->
    ?ltl_label:string ->
    ?custom_label:string ->
    ?bg_transparent:bool ->
    ?bg_color:int ->
    ?font_color:int ->
    ?shape_color:int ->
    graph ->
    Buchi.automaton ->
    string ->
    unit
end

module Visualizer : VisualizerType = struct
  open Ltl
  open Buchi

  (** a module for graph *)
  module G =
    Imperative.Digraph.ConcreteLabeled
      (* takes two modules as arguments, one for vertex label, one for edge
         label *)
      (struct
        (* for vertex label *)
        type t = int

        let compare = Int.compare
        let hash = Hashtbl.hash
        let equal = ( = )
      end)
      (struct
        (* for edge label *)
        type t = string

        let compare = String.compare
        let default = ""
      end)

  type graph = G.t (* G.t is the type of the graph *)

  (* helper. Given a list of states [state_list], output a graph [g] that only
     contains corresponding vertices (no edge). The name of a vertex is "q"^"the
     int of the state" *)

  let rec help_init_vertex (state_list : Buchi.state list) : graph =
    match state_list with
    | [] -> G.create ()
    | n :: rest ->
        let g = help_init_vertex rest in
        G.add_vertex g n;
        g

  let rec help_initial_state (initial_states : Buchi.state list) (g : graph) :
      graph =
    match initial_states with
    | [] -> g
    | n :: rest ->
        let new_g = help_initial_state rest g in
        G.add_vertex new_g (-n - 1);
        new_g

  let init_vertex (a : Buchi.automaton) : graph =
    let g = help_init_vertex a.states in
    help_initial_state a.initial_states g

  (* Given a state [state] and a graph [g], output the vertex [v] matching
     [state]. Requires: there must be a vertex matching the state *)
  let find_vertex (state : Buchi.state) (g : graph) : G.vertex =
    Option.get
      (G.fold_vertex
         (fun v acc ->
           match acc with
           | Some found -> Some found
           | None -> if v = state then Some v else None)
         g None)

  let help_initial_state_edge (a : Buchi.automaton) (g : graph) =
    List.fold_left
      (fun some_g s ->
        (*G.add_vertex some_g (~-s - 1);*)
        G.add_edge_e some_g (G.E.create (~-s - 1) "" s);
        some_g)
      g a.initial_states

  let rec help_init_edge (trans_list : Buchi.transition list) (g : graph) :
      graph =
    match trans_list with
    | [] -> g
    | t :: rest ->
        let new_g = help_init_edge rest g in
        let source = find_vertex t.from new_g in
        let destination = find_vertex t.to_ new_g in
        let label = Ltl.to_string t.condition in
        G.add_edge_e new_g (G.E.create source label destination);
        new_g

  (* Given a list of transitions [trans_list] and a graph [g], output the graph
     with all the transition added as edges. Requires: whenever a state appears
     as the endpoint of a transition, the corresponding vertex must exist in the
     graph*)
  let init_edge (a : Buchi.automaton) (g : graph) : graph =
    let new_g = help_init_edge a.transitions g in
    help_initial_state_edge a new_g

  (** Given automaton [a], output the graph *)
  let visualize (a : Buchi.automaton) : graph =
    let g = init_vertex a in
    init_edge a g

  let output_png ?(show_start = false) ?(ltl_label = "") ?(custom_label = "")
      ?(bg_transparent = false) ?(bg_color = 16777215) ?(font_color = 0)
      ?(shape_color = 0) (g : graph) (a : Buchi.automaton) (png_name : string) :
      unit =
    let file = open_out_bin "graph.dot" in
    let module Dot = Graphviz.Dot (struct
      include G

      let graph_attributes _ =
        [
          `Label (custom_label ^ " " ^ ltl_label);
          `Concentrate true;
          `Center true;
          `Orientation `Portrait;
          `Bgcolor bg_color;
        ]
        @ if bg_transparent then [ `BgcolorWithTransparency 0l ] else []

      let default_vertex_attributes _ = []

      let vertex_name v =
        if v >= 0 then "q" ^ string_of_int v (* i -> "qi"*)
        else "start_" ^ string_of_int (-(v + 1))
      (* dummy for initial state*)

      let vertex_attributes v =
        (if List.mem v a.final_states then [ `Shape `Doublecircle ]
         else if v < 0 then
           if show_start then [ `Shape `Box ]
           else [ `Shape `Box; `Style `Invis ]
         else [ `Shape `Circle ])
        @ [ `Fontcolor font_color; `Color shape_color ]

      let default_edge_attributes _ =
        [ `Fontcolor font_color; `Color shape_color ]

      let edge_attributes (_, e, _) = [ `Label e ]
      let get_subgraph _ = None
    end) in
    Dot.output_graph file g;
    close_out file;
    ignore (Sys.command ("dot -Tpng graph.dot -o " ^ png_name ^ ".png"))
end
