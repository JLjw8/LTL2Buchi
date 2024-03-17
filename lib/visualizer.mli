(** visualizer takes in an LTL expression and returns an ocamlgraph *)

module type VisualizerType = sig
  open Buchi

  type graph

  val visualize : Buchi.automaton -> graph
  (** Given automaton [a], [visualize a] outputs the graph representation of [a]*)

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
  (** Given graph [g], automaton [a], and string [png_name],
      [output_png g a png_name] outputs it a png file named [png_name].png at
      the root directory. Parameters: [show_start=false]: whether to connect a
      box to the initial state; [ltl_label=""]: the string representation of the
      LTL formula; [custom_label=""]: the string specified by the user to
      precede [ltl_label] in the image; [bg_transparent=false]: whether to use
      transparent background; [bg_color=16777215]: 10-base RGB value of
      background color (default is white); [font_color=0]: 10-base RGB value of
      font color (default is black); [shape_color=0]: 10-base RGB value of shape
      color (arrows and circles) (default is black);*)
end

module Visualizer : VisualizerType
