(**
   Data structure used for handling the code in similarities processing.
   @author Denis Merigoux
*)

(**{2 Type definitions}*)

type source_files = string * string

type cfg_node_id = int * Control_flow_c.Key.t
(**
   Each node carries information about its source files and a unique identifier.
*)

type node_info = {
  source_files : source_files (**(before,after)*);
  cfg_id : cfg_node_id;
}

module Identifiers = Set.Make(String)

(**{3 Normal code}*)

(** The last int of the [code_fragment] is the index of the graph in the code.*)
type code_fragment = CFGDiff.diff_graph * source_files * int
type t = code_fragment list

(**
   Each code fragment comes with a list of nodes marked as interesting.
*)
type annotated_code = (code_fragment * Control_flow_c.Key.t list) list


type forest_node = CFGDiff.diff_node * node_info
type forest = forest_node list

(**{2 Printing and debugging }*)

let stringify_cfg_node_id ((graph,key) : cfg_node_id) : string =
  Printf.sprintf "Graph #%d, Key #%d" graph key

let stringify_source_files ((before,after) : source_files) : string =
  Printf.sprintf "File before: %s \nFile after: %s" before after

(** Returns the number of nodes of all the CFG in the code. *)
let count_nodes (code : t) : int =
  let count = ref 0 in
  List.iter (fun (graph,_,_) ->
      count:=!count+Control_flow_c.KeyMap.cardinal (graph#nodes);
    ) code;
  !count

(**{2 Utilities}*)

(** Gets the prefix ([After], [Before] or [Both]) in a node.*)
let get_prefix (((prefix,node),node_info) : forest_node) : CFGDiff.prefix =
  prefix

let get_info ((_,node_info) : forest_node) : node_info = node_info

(**{2 Format converters}*)

(**
   Remove all graphs not containing any modified nodes.
*)
let programs_to_code (code_before : CFGDiff.numbered_diff_program option list)
                     : t =
    let program_to_code (accu : t) program : t = match program with
    | Some(numbered_graphs, (before,after)) ->
        let graph_to_code (accu : t) (diff_graph, index) : t =
            accu@[(diff_graph, (before, after), index)]
        in
        List.fold_left graph_to_code accu numbered_graphs
    | None -> accu
    in
    List.fold_left program_to_code [] code_before


let code_to_forest ( code : t) : forest =
  let forest = ref [] in
  let get_modified_forest_from_code_fragment
    (code_fragment, (before,after), cfg_index) =
    let nodes = code_fragment#nodes in
    let add_node key diff_node accu =
      let node_info = {
        source_files = (before,after);
        cfg_id = (cfg_index,key);
      } in
      (diff_node,node_info)::accu
    in
    forest := (Control_flow_c.KeyMap.fold add_node nodes [])@(!forest)
  in
  List.iter get_modified_forest_from_code_fragment code;
  !forest

(**{2 Code filters}*)

(** Removes all the modified nodes from a forest. *)
let remove_modified_nodes (forest : forest) : forest =
  List.filter (fun ((prefix,_),_) ->
      match prefix with
      | CFGDiff.After
      | CFGDiff.Before ->
        false
      | CFGDiff.Both ->
        true) forest

(** Returns [(removed_nodes,added_nodes,context_nodes)]. *)
let filter_modified_code
    (forest : forest)
  : forest * forest * forest =
  let is_removed ((node,_) : forest_node) : bool =
    match fst node with
    | CFGDiff.Before -> true
    | _ -> false
  in
  let is_added ((node,_) : forest_node) : bool =
    match fst node with
    | CFGDiff.After -> true
    | _ -> false
  in
  let is_context ((node,_) : forest_node) : bool =
  match fst node with
  | CFGDiff.Both -> true
  | _ -> false
  in
  (List.filter is_removed forest,
   List.filter is_added forest,
   List.filter is_context forest)

(** Helper function to copy nodes and egdes to a new graph. *)
let copy_nodes old_graph new_graph nodes filter_fun =
  List.iter (fun (key,node) ->
      new_graph#add_nodei key node;
    ) nodes;
  List.iter (fun (key,node) ->
      CFGDiff.KeyEdgeSet.iter (fun (key_prec,edge) ->
          try
            new_graph#add_arc ((key_prec,key), edge);
          with
          | Not_found -> ()
        ) (CFGDiff.KeyEdgeSet.filter filter_fun (old_graph#predecessors key));
      CFGDiff.KeyEdgeSet.iter (fun (key_suc,edge) ->
          try
            new_graph#add_arc ((key,key_suc), edge)
          with
          | Not_found -> ()
        ) (CFGDiff.KeyEdgeSet.filter filter_fun (old_graph#successors key))
    ) nodes

let filter_before (_, edge) = match edge with
| CFGDiff.Before | CFGDiff.Both -> true
| CFGDiff.After -> false

let filter_after (_, edge) = match edge with
| CFGDiff.After | CFGDiff.Both -> true
| CFGDiff.Before -> false

(**
   Splits  merged CFG into its component graphs before and after modification.
*)
let dissociate_merged_CFG
    (graph : CFGDiff.diff_graph)
  : CFGDiff.diff_graph * CFGDiff.diff_graph =
  let nodes = graph#nodes in
  let before_graph = new CFGDiff.CEG.ograph_mutable in
  let after_graph = new CFGDiff.CEG.ograph_mutable in
  let nodes_before = ref [] in
  let nodes_after = ref [] in
  Control_flow_c.KeyMap.iter (fun key node ->
      match node with
      | (CFGDiff.Before,_) ->
        nodes_before:=(key,node)::!nodes_before
      | (CFGDiff.After,_) ->
        nodes_after:=(key,node)::!nodes_after
      | (CFGDiff.Both,_) ->
        nodes_before:=(key,node)::!nodes_before;
        nodes_after:=(key,node)::!nodes_after
    ) nodes;
  copy_nodes graph before_graph !nodes_before filter_before;
  copy_nodes graph after_graph !nodes_after filter_after;
  (before_graph,after_graph)

(**
   Uses Coccinelle's C visitors to get all the variable names
   from a particular node.
*)
let extract_names_visitor (node : CFG.node) : Identifiers.t =
  let names = ref [] in
  let visitor_name =
    {
      Visitor_c.default_visitor_c with
      Visitor_c.kname = (fun (k, _) name -> names := name::!names)
    } in
  begin
    Visitor_c.vk_node visitor_name node;
    (** [names] now contain the list of names contained in the node. *)
    List.fold_left
      (
        fun accu name -> match name with
          | Ast_c.RegularName(strname,_) ->
            Identifiers.add strname accu
          | _ -> accu
      )
      Identifiers.empty !names
  end
