(**
   Main data structure for spinfer. Similar to a CFG but the nodes are
   prefixed with Before, After or Both to account for the code modification.
   @author Denis Merigoux
*)

(**{2 Types and structures}*)

(**
   We use a CFG similar to Coccinelle's [Control_flow_c], but here the nodes
   carry diff information. The two [string] are the names of the source code
   files before and after modifications.
*)
module CFC = Control_flow_c
type prefix =

  | Before (** Present only before the modification. *)
  | After (** Present only after the modification. *)
  | Both (** Unchanged by the modification. *)

module ColoredEdge : Set.OrderedType with type t = prefix
module KeyEdgePair : Set.OrderedType with type t = CFC.Key.t * ColoredEdge.t
module KeyEdgeSet : Set.S with type elt = KeyEdgePair.t

module CEG : Ograph_extended.S with
  type key = CFC.Key.t and
  type 'a keymap = 'a CFC.KeyMap.t and
  type edge = prefix and
  type edges = KeyEdgeSet.t

type diff_node  = prefix * CFC.node
type diff_graph = diff_node CEG.ograph_mutable
type diff_program = diff_graph list * (string * string)

type numbered_diff_program = (diff_graph * int) list * (string * string)

val first_key : diff_graph -> CFC.Key.t
val last_key : diff_graph -> CFC.Key.t

val equal_diff_nodes : diff_node -> diff_node -> bool

(** {2 Printing} *)

val stringify_diff_prefix : prefix -> string

val stringify_CFGDiff_node : diff_node -> string

(**
   Gives the color of a diff node on a graph, green if added and red if removed.
*)
val border_color_of : diff_node -> string option

val color_of : diff_node -> string option

(**
   [print_diff_graph graph file] prints a {!type:CFGDiff.diff_graph} [graph]
   into [file], in a format readable by GraphViz visualizators. Modified nodes
   are colored.
*)
val print_diff_graph : diff_graph -> string -> unit

(**{2 Building the CFG diff}*)


(**
   [mark_program_nodes (before,after) diffs], for each [diff] of [diffs]:
   - marks corresponding nodes in the [before] graph as
   {!const:CFGDiff.prefix.Before} if the diff removes them and marks other nodes
   in the [before] graph as {!const:CFGDiff.prefix.Both};
   - marks corresponding nodes in the [after] graph as
   {!const:CFGDiff.prefix.After} if the diff removes them and marks other nodes
   in the [after] graph as {!const:CFGDiff.prefix.Both}.

   The output is thus two {!type:CFGDiff.diff_graph}[s]; the [before] contains
   info on the removed nodes and the [after] contains info on the added nodes.
*)
val mark_program_nodes : (CFG.program * CFG.program) -> Diff.t list ->
  (diff_program * diff_program)

val pair_functions : diff_graph list -> diff_graph list
   -> (diff_graph * diff_graph) list
(**
   [merge_diff before after] performs a parallel broad-first traversal of the
   [before] and [after] graphs and tries to build a single diff graph containing
   info on both the removed and added nodes at the correct place.
*)
val merge_diff : diff_program -> diff_program -> int -> Diff.t list
    -> numbered_diff_program option * int

val fun_header_of_graph : diff_graph -> Ast_c.definition option


val check_if_cocci_created : Control_flow_c.node2 -> bool