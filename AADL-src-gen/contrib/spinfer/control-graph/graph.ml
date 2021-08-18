module CFC = Control_flow_c


(** Sets of CFG nodes ids. *)
module NodeIdSet = Set.Make(struct
    type t = Code.cfg_node_id
    let compare = compare
end)


type dom_tree = {
    key: CFGDiff.CEG.key;
    children: dom_tree list;
}

(**{2 The domination computing algorithm}*)

(**
   Computes the dominators in a graph. Returns an association list where the
   keys are the node in the graph and the values are the sets of nodes that
   dominate the key.
*)
let compute_dominators_graph
        (graph : 'a CFGDiff.CEG.ograph_mutable)
        (graph_id : int)
        (first_key : CFGDiff.CEG.key)
        (post_domination: bool)
    : (CFC.Key.t * NodeIdSet.t) list =
    let dominators : (CFC.Key.t, NodeIdSet.t) Hashtbl.t =
      Hashtbl.create 200
    in
    let set_of_all_nodes = CFC.KeyMap.fold (fun key _ accu ->
        NodeIdSet.add (graph_id, key) accu
    ) graph#nodes NodeIdSet.empty
    in
    try
        (** We initialize the set of dominators for each node. *)
        let nodes_wo_first = Utils.assoc_left
            (CFC.KeyMap.bindings
                (CFC.KeyMap.filter (fun key _ -> key <> first_key) graph#nodes)
            )
        in
        Hashtbl.add dominators first_key
            (NodeIdSet.singleton (graph_id, first_key));
        List.iter (fun key ->
            Hashtbl.add dominators key set_of_all_nodes
        ) nodes_wo_first;
        (** Then we iterate until the sets are stabilized *)
        let is_changed = ref true in

        let iter_order = if post_domination
            then List.rev nodes_wo_first (* More performance with that order *)
            else nodes_wo_first
        in
        while(!is_changed) do
            is_changed := false;
            List.iter (fun key ->
                let inter_dom_keys = if not post_domination
                then
                    CFGDiff.KeyEdgeSet.fold (fun (key_prec, _) accu ->
                        NodeIdSet.inter accu (Hashtbl.find dominators key_prec)
                    ) (graph#predecessors key) set_of_all_nodes
                else
                    CFGDiff.KeyEdgeSet.fold (fun (key_succ, _) accu ->
                    (* Error node are not connected to the Exit node,
                    so they have no successors *)
                        try
                            NodeIdSet.inter accu
                                (Hashtbl.find dominators key_succ)
                        with Not_found ->
                            accu
                    ) (graph#successors key) set_of_all_nodes
                in
                let new_dominators = NodeIdSet.add (graph_id, key) inter_dom_keys in
                if (Hashtbl.find dominators key) <> new_dominators
                then begin
                    Hashtbl.replace dominators key new_dominators;
                    is_changed:=true;
                end
            ) iter_order;
        done;
        Hashtbl.fold (fun key dominator_keys accu ->
            (key, dominator_keys)::accu
        ) dominators []
    with Not_found -> CFC.KeyMap.fold (fun key _ accu ->
        (key, (NodeIdSet.singleton (graph_id, key)))::accu
    ) graph#nodes []

(**{2 API }*)

let get_dominators
  (graphs : ('a CFGDiff.CEG.ograph_mutable * int) list)
  (first_key : 'a CFGDiff.CEG.ograph_mutable -> CFGDiff.CEG.key)
  (last_key : 'a CFGDiff.CEG.ograph_mutable -> CFGDiff.CEG.key)
  : (Code.cfg_node_id * NodeIdSet.t) list * (Code.cfg_node_id * NodeIdSet.t) list =
  let dominators = List.flatten (
    List.map (fun (graph, graph_id) ->
      List.map
        (fun (key,doms) -> ((graph_id,key),doms))
        (compute_dominators_graph graph graph_id (first_key graph) false)
    ) graphs
  ) in
  let postdominators = List.flatten (
    List.map (fun (graph, graph_id) ->
      List.map
        (fun (key,doms) -> ((graph_id,key),doms))
        (compute_dominators_graph graph graph_id (last_key graph) true)
    ) graphs
  ) in
  (dominators, postdominators)


(** Compute the strict domination sets of a colored edge graph *)
let get_domination_sets (graph: 'a CFGDiff.CEG.ograph_mutable)
                        (first_key: CFGDiff.CEG.key)
                        : (CFGDiff.CEG.key * Utils.IntSet.t) list =
    let domination_sets = Hashtbl.create 30 in
    let dominators_list = compute_dominators_graph graph 0 first_key false in
    Control_flow_c.KeyMap.iter (fun key _ ->
        Hashtbl.add domination_sets key (Utils.IntSet.empty)
    ) graph#nodes;

    List.iter (fun (key, dominators) ->
        NodeIdSet.iter (fun (_, dominator) ->
            if dominator <> key (* Strict domination *)
            then begin
                let previous_set = Hashtbl.find domination_sets dominator in
                let new_set = Utils.IntSet.add key previous_set in
                Hashtbl.replace domination_sets dominator new_set
            end
        ) dominators
    ) dominators_list;

    Hashtbl.fold (fun dom set accu -> (dom, set)::accu) domination_sets []


(** Compute the domination tree of a colored edge graph.
    Children are ordered in respect to the compare function
*)
let compute_dominator_tree (graph: 'a CFGDiff.CEG.ograph_mutable)
                           (first_key: CFGDiff.CEG.key)
                           (compare:
                             CFGDiff.CEG.key * 'a -> CFGDiff.CEG.key * 'a -> int
                           )
                           : dom_tree =
    let dom_sets = get_domination_sets graph first_key in
    let nodes = graph#nodes in

    let rec build_tree key =
        let immediately_dominated_nodes =
            let key_set = List.assoc key dom_sets in
            Utils.IntSet.fold (fun dom_keys accu ->
                let other_set = List.assoc dom_keys dom_sets in
                if (other_set <> key_set) && (Utils.IntSet.subset other_set key_set)
                then Utils.IntSet.diff accu other_set
                else accu
            ) key_set key_set
        in
        if Utils.IntSet.is_empty immediately_dominated_nodes
        then {key=key; children=[]}
        else
            let children =
                List.map build_tree
                         (Utils.IntSet.elements immediately_dominated_nodes)
            in
            let compare_children a b =
                let get_pattern_node key =
                    Control_flow_c.KeyMap.find key nodes
                in
                let a_node = get_pattern_node a.key in
                let b_node = get_pattern_node b.key in
                compare (a.key, a_node) (b.key, b_node)
            in
            {
                key=key;
                children=List.sort compare_children children
            }
    in

    build_tree first_key


let print_dominator_tree (tree: dom_tree) (filename: string) : unit =
    let str_of_children (node: dom_tree) =
        let keys =
            List.map (fun child -> string_of_int child.key) node.children
        in
        let str_nodes =
            "{rank=same; " ^
            (String.concat "; " keys) ^
            "}\n"
        in
        let str_edges =
            let edges_list =
                List.map (Printf.sprintf "%d -> %s;\n" node.key) keys
            in
            String.concat "" edges_list
        in
        str_nodes ^ str_edges
    in

    let rec str_body (accu: string) (node: dom_tree) =
        let str_current = str_of_children node in
        List.fold_left str_body (accu ^ str_current) node.children
    in

    let chan = open_out filename in
    output_string chan "digraph dom_tree {\n";
    output_string chan (str_body "" tree);
    output_string chan "}\n";
    close_out chan


let compute_context_set (graphs: (CFGDiff.diff_graph * int) list)
                        (reverse: bool)
                        : (Code.cfg_node_id * NodeIdSet.t) list =

    let extract_context_set id graph
                            : (Code.cfg_node_id * NodeIdSet.t) list =
        let context_sets = Hashtbl.create 30 in
        let remove_fake = CFGDiff.KeyEdgeSet.filter (fun (key, _) ->
            let node = Control_flow_c.KeyMap.find key graph#nodes in
            if (Control_flow_c.unwrap (snd node) = Control_flow_c.Fake)
            then false
            else true
        )
        in
        Control_flow_c.KeyMap.iter (fun key _ ->
            Hashtbl.add context_sets key NodeIdSet.empty
        ) graph#nodes;
        let is_changed = ref true in
        while !is_changed do
            is_changed := false;
            Control_flow_c.KeyMap.iter (fun key node ->
                let previous_set = Hashtbl.find context_sets key in
                let context_nodes =
                    if reverse
                    then graph#successors key
                    else graph#predecessors key
                in
                begin match fst node with
                | CFGDiff.Both ->
                    Hashtbl.replace context_sets key
                        (NodeIdSet.singleton (id, key))
                | _ ->
                    let new_set = CFGDiff.KeyEdgeSet.fold (fun (key_pred, _) accu ->
                        NodeIdSet.union accu
                            (Hashtbl.find context_sets key_pred)
                    ) (remove_fake context_nodes) NodeIdSet.empty
                    in
                    Hashtbl.replace context_sets key new_set
                end;
                if (Hashtbl.find context_sets key) <> previous_set
                    then is_changed := true
            ) graph#nodes
        done;
        Hashtbl.fold (fun key context_set accu ->
            ((id, key), context_set)::accu
        ) context_sets []
    in

    List.flatten (
        List.map (fun (graph, graph_id) ->
            extract_context_set graph_id graph
        ) graphs
    )
