(**
   Code handling during similarities detection.
   @author Denis Merigoux*)

(**{2 Type definitions}*)

(** The format is [(file_before,file_after)]. *)
type source_files = string * string

(**
   Information to locate a particular node inside a [Code.t] structure.
*)
type cfg_node_id = int * Control_flow_c.Key.t

(**
   Each node carries information about its source files and information to locate
   them inside a [forest] or a [Code.t] structure.
*)
type node_info = {
  source_files : source_files (**(before,after)*);
  cfg_id : cfg_node_id;
}

module Identifiers : Set.S with type elt = string

(**{3 Normal code}*)

(** The last int of the [code_fragment] is the index of the graph in the code.*)
type code_fragment = CFGDiff.diff_graph * source_files * int

(** Type of the code. *)
type t = code_fragment list

(** A code can be annotated with a list of interesting nodes. *)
type annotated_code = (code_fragment * Control_flow_c.Key.t list) list

(** A lonely code node with some information attached to it. *)
type forest_node = CFGDiff.diff_node * node_info

(** Data structure used in similarity detection. (IMPORTANT) *)
type forest = forest_node list

(**{2 Printing and debugging }*)

val stringify_cfg_node_id : cfg_node_id -> string
val stringify_source_files : source_files -> string

(** Used for info and debug. *)
val count_nodes : t -> int

(**{2 Utilities}*)

(** Gets the prefix ([After], [Before] or [Both]) in a node.*)
val get_prefix : forest_node -> CFGDiff.prefix

(** Gets the [node_info] in a node. *)
val get_info : forest_node -> node_info

(**{2 Format converters}*)

(** Transforms the output of {!module:CFGDiff} into code. *)
val programs_to_code : CFGDiff.numbered_diff_program option list -> t

(**
   Transforms structured code into forests to be proecessed by the clone
   detection algorithm.
*)
val code_to_forest : t -> forest

(**{2 Filter code}*)

(** Removes all the modified nodes in a forest. *)
val remove_modified_nodes : forest -> forest

(** Returns (removed_nodes,added_nodes)*)
val filter_modified_code : forest -> forest * forest * forest

(** Returns all the identifiers (Coccinelle's names) contained in a CFG node. *)
val extract_names_visitor : CFG.node -> Identifiers.t

(** Splits a merged CFG into its original graphs [(before,after)]*)
val dissociate_merged_CFG : CFGDiff.diff_graph ->
  CFGDiff.diff_graph * CFGDiff.diff_graph
