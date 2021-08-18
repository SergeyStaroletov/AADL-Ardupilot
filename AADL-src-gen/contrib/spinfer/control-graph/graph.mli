(**
   Uses graph dominators to determine execution path orders in a merged CFG.
   @author Denis Merigoux
*)

module NodeIdSet : Set.S with type elt = Code.cfg_node_id

type dom_tree = {
    key: CFGDiff.CEG.key;
    children: dom_tree list;
}

val compute_dominators_graph :
    ('a CFGDiff.CEG.ograph_mutable) ->
    (int) ->
    (CFGDiff.CEG.key) ->
    (bool) ->
    (Control_flow_c.Key.t * NodeIdSet.t) list

val get_dominators :
  ('a CFGDiff.CEG.ograph_mutable * int) list ->
  ('a CFGDiff.CEG.ograph_mutable -> CFGDiff.CEG.key) ->
  ('a CFGDiff.CEG.ograph_mutable -> CFGDiff.CEG.key) ->
  (Code.cfg_node_id * NodeIdSet.t) list * (Code.cfg_node_id * NodeIdSet.t) list

val get_domination_sets :
    'a CFGDiff.CEG.ograph_mutable ->
    CFGDiff.CEG.key ->
    (CFGDiff.CEG.key * Utils.IntSet.t) list

val compute_dominator_tree :
    'a CFGDiff.CEG.ograph_mutable ->
    CFGDiff.CEG.key ->
    (CFGDiff.CEG.key * 'a -> CFGDiff.CEG.key * 'a -> int) ->
    dom_tree

val print_dominator_tree : dom_tree -> string -> unit

val compute_context_set: (CFGDiff.diff_graph * int) list -> bool ->
    (Code.cfg_node_id * NodeIdSet.t) list
