(** @author Denis Merigoux *)

(**{2 Type definitions}*)
module CFC = Control_flow_c

type prefix =
  | Before
  | After
  | Both

module ColoredEdge : Set.OrderedType with type t = prefix = struct
    type t = prefix
    let compare = compare
end

module KeyEdgePair : Set.OrderedType with type t = CFC.Key.t * ColoredEdge.t =
struct
  type t = CFC.Key.t * ColoredEdge.t
  let compare = compare
end

module KeyEdgeSet : Set.S with type elt = KeyEdgePair.t = Set.Make (KeyEdgePair)

module CEG = Ograph_extended.Make (CFC.Key) (CFC.KeySet) (CFC.KeyMap)
    (ColoredEdge) (KeyEdgePair) (KeyEdgeSet)

type diff_node  = prefix * CFG.node
type diff_graph = diff_node CEG.ograph_mutable
type diff_program = diff_graph list * (string * string)

type numbered_diff_program = (diff_graph * int) list * (string * string)

exception ModifiedNodeNotFound of string * Diff.position option

let first_key (graph : diff_graph) : CFC.Key.t =
    let is_first_node (_, node) =
        begin match CFC.unwrap (snd node) with
            | Control_flow_c.TopNode -> true
            | _ -> false
        end
    in
    let nodes = graph#nodes in
    let (key_first_node, _) =
        List.find is_first_node (CFC.KeyMap.bindings nodes)
    in
    key_first_node

let last_key (graph : diff_graph) : CFC.Key.t =
    let is_last_node (_, node) =
        begin match CFC.unwrap (snd node) with
            | Control_flow_c.Exit
            | Control_flow_c.EndNode -> true
            | _ -> false
        end
    in
    let nodes = graph#nodes in
    let (key_last_node, _) =
        List.find is_last_node (CFC.KeyMap.bindings nodes)
    in
    key_last_node

(**{2 Printing and debugging}*)
let stringify_diff_prefix (prefix : prefix) : string =
  match prefix with
  | Before -> "- "
  | After -> "+ "
  | Both -> ""

let stringify_CFGDiff_node ((prefix,node) : diff_node) : string =
  let strprefix = stringify_diff_prefix prefix in
  let strnode = Cocci_addon.stringify_CFG_node node in

  strprefix^"|"^strnode

let border_color_of node = match node with
  | (After,_) -> Some("green4")
  | (Before,_) -> Some("red4")
  | (Both,_) -> None

let color_of node = match node with
  | (After,_) -> Some("darkolivegreen2")
  | (Before,_) -> Some("brown1")
  | (Both,_) -> None

let print_diff_graph (diff_graph : diff_graph) (filename : string) =
  CEG.print_ograph_mutable_generic diff_graph
    (None)
    (fun (key,((_,node') as node)) ->
       (Cocci_addon.stringify_CFG_node node',border_color_of node,color_of node))
    filename
    false

(* FIXME: Remove this override when printing edges is implemented in coccinelle *)
let print_diff_graph (diff_graph: diff_graph) (filename : string) =
    let fnode = (fun (key,((_,node') as node)) ->
       (Cocci_addon.stringify_CFG_node node',border_color_of node,color_of node))
    in

    let g = diff_graph in
    let label = None in
    Common.with_open_outfile filename (fun (pr,_) ->
      pr "digraph misc {\n" ;
      pr "size = \"10,10\";\n" ;
      (match label with
        None -> ()
      | Some x -> pr (Printf.sprintf "label = \"%s\";\n" x));

      let print_node k node =
        let (str,border_color,inner_color) = fnode (k, node) in
        let color =
	  match inner_color with
	    None ->
	      (match border_color with
	        None -> ""
	      | Some x -> Printf.sprintf ", style=\"setlinewidth(3)\", color = %s" x)
	  | Some x ->
	      (match border_color with
	        None -> Printf.sprintf ", style=\"setlinewidth(3),filled\", fillcolor = %s" x
	      | Some x' -> Printf.sprintf ", style=\"setlinewidth(3),filled\", fillcolor = %s, color = %s" x x') in
       (* so can see if nodes without arcs were created *)
        pr ( Printf.sprintf "%d [label=\"%s   [%d]\"%s];\n"
          k (String.escaped str) k color
        )
      in
      let nodes = g#nodes in
      CFC.KeyMap.iter print_node nodes;

      let print_edges k node =
        let print_edge (j, edge) =
        let color = match edge with
        | Before -> "red4"
        | After -> "green4"
        | Both -> "black"
        in
        pr (Printf.sprintf "%d -> %d [color=%s];\n" k j color) in
        KeyEdgeSet.iter print_edge (g#successors k) in

      CFC.KeyMap.iter print_edges nodes;
      pr "}\n" ;
      );
    ()


(**{2 Handling {!module:Diff} results and marking nodes}*)

(**
   Retrieves in a graph all the nodes which lie in a given line position in
   the source file.
*)
let get_nodes_from_lines (position : Diff.position)
    (cfg : CFG.control_flow_graph) : Control_flow_c.Key.t list =

  let supported_node node : bool =
    match node with
    (** The following nodes are not supported yet. *)
    | Control_flow_c.IfdefHeader(_)
    | Control_flow_c.IfdefElse(_)
    | Control_flow_c.IfdefEndif(_)
    | Control_flow_c.IfdefIteHeader(_)
    | Control_flow_c.DefineHeader(_)
    | Control_flow_c.DefineType(_)
    | Control_flow_c.DefineExpr(_)
    | Control_flow_c.DefineTodo
    | Control_flow_c.Include(_)
    | Control_flow_c.DefineDoWhileZeroHeader(_)
    | Control_flow_c.PragmaHeader(_,_)
    | Control_flow_c.MacroTop(_,_,_)
    | Control_flow_c.Asm(_,_)
    | Control_flow_c.MacroStmt(_,_)
    | Control_flow_c.Exec(_,_)
    | Control_flow_c.ExprStatement(_,(None,_)) -> false
    | _ -> true
  in
  let key_list = ref [] in
  let check_node_and_add_to_list key node =
    (**
       Both functions below can raise {!exception:CFG.NotInC} or
       {!exception:CFG.NoLineFound} exceptions.
    *)
    let lines = CFG.get_line_from_node node in

    let pos_start, pos_end = Diff.position_to_pair position in
    let lines_start, lines_end = Diff.position_to_pair lines in

    (* A node can be on multiple lines so we check if
        the position and lines do overlap *)
    if (pos_start <= lines_end) && (lines_start <= pos_end)
       && (supported_node (Control_flow_c.unwrap node))
    then key_list := key::!key_list
  in let iter_keymap key node =
       begin try check_node_and_add_to_list key node with
         (** We simply drop the nodes with exceptions, they are not relevant *)
         | Debug.NotInC(_)
         | CFG.NoLineFound -> ()
       end
  in
  let nodes = cfg#nodes in
  Control_flow_c.KeyMap.iter iter_keymap nodes;
  (** Returns the list accumulated by [check_node_and_add_to_list] *)
  !key_list

let remove_notfound_CFG_nodes_positions
  ((program,filename) : CFG.program) (positions : Diff.position list)
    : Diff.position list =
  let requested_positions =
    List.map (fun x -> (Diff.position_to_pair x, x)) positions in
  let requested_positions = List.sort compare requested_positions in
  let supported_node node : bool =
    match Control_flow_c.unwrap node with
    (** The following nodes are not supported yet. *)
    | Control_flow_c.IfdefHeader(_)
    | Control_flow_c.IfdefElse(_)
    | Control_flow_c.IfdefEndif(_)
    | Control_flow_c.IfdefIteHeader(_)
    | Control_flow_c.DefineHeader(_)
    | Control_flow_c.DefineType(_)
    | Control_flow_c.DefineExpr(_)
    | Control_flow_c.DefineTodo
    | Control_flow_c.Include(_)
    | Control_flow_c.DefineDoWhileZeroHeader(_)
    | Control_flow_c.PragmaHeader(_,_)
    | Control_flow_c.MacroTop(_,_,_)
    | Control_flow_c.Asm(_,_)
    | Control_flow_c.MacroStmt(_,_)
    | Control_flow_c.Exec(_,_)
    | Control_flow_c.ExprStatement(_,(None,_)) -> false
    | _ -> true in
  let program =
    let nodes = ref [] in
    List.iter
      (function cfg ->
	CFC.KeyMap.iter
	  (fun _ x -> if supported_node x then nodes := x :: !nodes)
	  cfg#nodes)
      program;
    !nodes in
  let lines_of_program =
    List.fold_left
      (fun prev cur ->
	try CFG.get_line_from_node cur :: prev
	with
         (** We simply drop the nodes with exceptions, they are not relevant *)
         | Debug.NotInC(_) | CFG.NoLineFound -> prev)
      [] program in
  let lines_of_program = List.map Diff.position_to_pair lines_of_program in
  let lines_of_program = List.sort compare lines_of_program in
  let fail pos =
    Debug.print
      (Printf.sprintf
	 "[WARNING] File : %s, line(s) : %s.\nCorresponding nodes to the modified line(s) are not found in the CFG. It could stem from a Coccinelle parsing error."
	 filename (Diff.stringify_position pos)) in
  let rec loop prog = function
      [] -> []
    | (((starter,ender),pos)::rest) as cur ->
	match prog with
	  [] -> fail pos; []
	| (pstarter,pender)::prest ->
	    if starter <= pender && ender >= pstarter
	    then pos :: loop prog rest
	    else
	      if pstarter <= starter
	      then loop prest cur (* keep looking *)
	      else (* give up on this position *)
		begin
		  fail pos;
		  loop prog rest
		end in
  loop lines_of_program requested_positions

(**
   Because node keys are relative to a specific flow graph in the list that
   makes a program, we need to keep track of which modified node corresponds to
   which CFG. The output is a list which contains as many elements as CFG in
   the program. Each element of the list is a list of diff-like object relative
   to nodes in a particular CFG.

   The lines in the {!type:Diff.position} [list] are assumed to be sorted in
   ascending order so we can search the program one CFG after the other.
*)
let build_modified_nodes_list
    ((program,filename) : CFG.program) (original_positions : Diff.position list)
  : (Control_flow_c.Key.t list) list =
  (** The next lines is for debug *)

  Debug.data := {!Debug.data with Debug.current_file = filename };
  let filt_positions =
     remove_notfound_CFG_nodes_positions (program,filename) original_positions
  in
  let rec traverse_positions
      program positions current_accu result_accu current_program_index =
    (**
       The [current_accu] is the list of modified nodes in the current program.
       The [result_accu] is the list of lists of modified nodes in all the
       programs.
    *)
      match positions with
        (** Keep the same order as the program *)
        | [] -> List.rev (current_accu::result_accu)
        | position::rest ->
          let flow_graph = List.nth program current_program_index in
          let modified_nodes = get_nodes_from_lines position flow_graph in
          if (List.length modified_nodes = 0 &&
            current_program_index < (List.length program - 1)) then
            (**
               The nodes we search are no more in the CFG, we move to the next
               and store the result for this CFG.
            *)
            let new_result_accu = current_accu::result_accu in
            traverse_positions
              program positions [] new_result_accu (current_program_index +1)
          else
            (**
               We append the found nodes in the [current_accu] and try the next
               position on the same CFG.
            *)
            let new_current_accu = (List.rev modified_nodes)@current_accu in
            traverse_positions
              program rest new_current_accu result_accu current_program_index
    in let almost_result = traverse_positions program filt_positions [] [] 0
    in
  (**
     [almost_result] is the list of lists required but one detail is missing:
     we need to fill it with empty lists to match the length of the program.
  *)
  let rec empty_list n =
    if (n>1) then
      (empty_list (n-1))@[[]]
    else [[]]
  in
  let n = (List.length program) - (List.length almost_result) in
  let result = if (List.length program = 0) then
      []
    else begin
      if (n>0) then almost_result@(empty_list n) else almost_result
    end

  in

    let result_plus = ref [] in
    List.iteri(fun i modified_nodes_list ->
        let graph = List.nth program i in
        let nodes = CFC.KeyMap.bindings graph#nodes in
        let new_modified = List.fold_left (fun accu key_node ->
            let node = List.assoc key_node nodes  in
            match CFC.unwrap node with
            | CFC.IfHeader _ ->
                let labels_if = CFC.extract_labels node in
                let true_false_nodes = ref [] in
                List.iter (fun (key, node) ->
                    if CFC.extract_labels node = labels_if
                    then begin
                        (* Tag branch domination nodes like their header. *)
                        match CFC.unwrap node with
                        | CFC.IfHeader(_,_)
                        | CFC.TrueNode(_)
                        | CFC.FalseNode
                        | CFC.FallThroughNode
                        | CFC.EndStatement(_) ->
                            true_false_nodes := key :: !true_false_nodes
                        | _ -> ()
                    end
                    else ()
                ) nodes;
                !true_false_nodes @ accu
            | _ -> key_node :: accu
        ) [] modified_nodes_list
        in
        result_plus := new_modified :: !result_plus;
    ) result;
    List.rev !result_plus

(**
   For each diff in the list, extract the positions of lines which are removed
   or added. The output is of the form (removed list,added list).
*)
let diff_list_to_added_or_removed_lines (diffs : Diff.t list) :
  (Diff.position list * Diff.position list) =
  let rec traverse_diff_list diffs (accu_removed,accu_added) =
    match diffs with
    | [] -> (accu_removed,accu_added)
    | diff::rest ->
      begin match diff.Diff.operation with
        | Diff.Added ->
          traverse_diff_list rest (accu_removed,diff.Diff.after::accu_added)
        | Diff.Deleted ->
          traverse_diff_list rest (diff.Diff.before::accu_removed,accu_added)
        | Diff.Changed ->
          traverse_diff_list rest
            (diff.Diff.before::accu_removed,diff.Diff.after::accu_added)
      end in
  traverse_diff_list diffs ([],[])

let check_if_cocci_created (node2 : Control_flow_c.node2) : bool =
    match node2 with
    | Control_flow_c.Enter
    | Control_flow_c.LoopFallThroughNode
    | Control_flow_c.FallThroughNode
    | Control_flow_c.AfterNode(_)
    | Control_flow_c.FalseNode
    | Control_flow_c.TrueNode(_)
    | Control_flow_c.InLoopNode
    | Control_flow_c.Fake
    | Control_flow_c.CaseNode(_)
    | Control_flow_c.EndStatement(_) -> true
    | _ -> false



(* TODO: patch before and remove this function *)
let fix_marking (graph: diff_graph) : unit =
    let nodes =
        Utils.assoc_right (CFC.KeyMap.bindings graph#nodes)
    in
    CFC.KeyMap.iter (fun key (_, node) ->
        match CFC.unwrap node with
        | CFC.TopNode | CFC.EndNode
        | CFC.Enter | CFC.Exit | CFC.ErrorExit -> ()
        | node2 ->
            if check_if_cocci_created node2
            then
                let labels = CFC.extract_labels node in
                let new_prefix, _ = List.find (fun (_, node) ->
                    not (check_if_cocci_created (CFC.unwrap node))
                    &&
                    labels = CFC.extract_labels node
                ) nodes
                in
                graph#replace_node (key, (new_prefix, node))
    ) graph#nodes


(**
   Outputs a {!type:CFGDiff.diff_graph} where {!type:CFGDiff.diff_node}
   are prefixed with [prefix] and the other with {!const:CFGDiff.prefix.Both}.
*)
let mark_cfg_nodes
    (cfg : CFG.control_flow_graph)
    (marked_keys : Control_flow_c.Key.t list)
    (prefix : prefix)
  : diff_graph =
  let keymap = cfg#nodes in
  let mark_node (key : Control_flow_c.Key.t) (node : CFG.node) : diff_node =
    if (List.exists (fun x -> x = key) marked_keys) then
     (prefix,node)
    else
      (Both,node)
  in
  (**
     Here we clone the control flow into a new graph with the same edges but
     nodes of a different type.
  *)
  let diff_keymap = Control_flow_c.KeyMap.mapi mark_node keymap in

  let diff_cfg  = new CEG.ograph_mutable in
  (** Fill the [diff_graph] with nodes and arcs*)
  Control_flow_c.KeyMap.iter
    (**
       Caution : the method [#add_nodei] is important because it allows to keep
       the same key for the nodes in the new graph. Indeed Coccinelle's CFG
       nodes keys are not always consecutive.
    *)
    (fun key node -> ignore (diff_cfg#add_nodei key node)) diff_keymap;
  (** Fill the diff_graph with edges *)
  let add_edges_to_diff key1 node1  =
    let add_edge (key2, _) =
      diff_cfg#add_arc ((key1,key2), Both);
    in let successors = cfg#successors key1 in
    Control_flow_c.KeyEdgeSet.iter add_edge successors
  in
  Control_flow_c.KeyMap.iter add_edges_to_diff keymap;
  fix_marking diff_cfg;
  diff_cfg

let traverse_cfg_list_and_mark_nodes
    ((program,filename) : CFG.program)
    (marked_keys_list : Control_flow_c.Key.t list list)
    (prefix : prefix)
  : diff_program =
  let rec traverse_cfg_list_and_mark_nodes_aux
      program marked_keys_list prefix
      (diff_program_accu : diff_graph list) =
    match (program,marked_keys_list) with
    | ([],[]) -> diff_program_accu
    | (cfg::rest_program,marked_keys::rest_keys) ->
      let new_diff_graph = mark_cfg_nodes cfg marked_keys prefix in
      traverse_cfg_list_and_mark_nodes_aux
        rest_program
        rest_keys
        prefix
        (new_diff_graph::diff_program_accu)
    | _ ->
      failwith (Printf.sprintf
                  "[traverse_cfg_list_and_mark_nodes] \
                   the cfg list and the marked keys list don't \
                   have the same size: %d and %d."
                  (List.length program)
                  (List.length marked_keys_list))
  in
  (**
     The diff_program list has been constructed the wrong way, we have to
     reverse it.
  *)
  let diff_program = traverse_cfg_list_and_mark_nodes_aux
      program marked_keys_list prefix [] in
  (List.rev diff_program,(filename,filename))

let mark_program_nodes
    ((before_program,after_program) : CFG.program * CFG.program)
    (diffs : Diff.t list)
  : diff_program * diff_program =
  (** The next three lines are for debug *)
  let (_,filename_before) = before_program in
  let (_,filename_after) = after_program in
  Debug.data := {!Debug.data with Debug.current_pair_of_files =
                                    (filename_before,filename_after)};
  let (removed_lines,added_lines) = diff_list_to_added_or_removed_lines diffs in
  (*Debug.print("removed_lines: " ^ (Dumper.dump removed_lines));*)
  let removed_nodes = build_modified_nodes_list before_program removed_lines in
  (*Debug.print("removed_nodes: " ^ (Dumper.dump removed_nodes));*)
  let added_nodes = build_modified_nodes_list after_program added_lines in
  (*Debug.print("added_nodes: " ^ (Dumper.dump added_nodes));*)
  let before_diff_program =
    traverse_cfg_list_and_mark_nodes before_program removed_nodes Before in
  let after_diff_program =
    traverse_cfg_list_and_mark_nodes after_program added_nodes After in
  (before_diff_program,after_diff_program)

(**{2 Building the {!type:CFGDiff.diff_graph} : parallel traversal}*)
(**
   Performs a semantic structural equality ignoring the parsing info using
   Cocinelle's function removing all the extra info in the nodes.
*)
let equal_diff_nodes
    ((prefix1,node1) : diff_node) ((prefix2,node2) :diff_node) : bool =
  match (prefix1,prefix2) with
  | (Before,Before)
  | (After,After)
  | (Both,Both) ->
    begin
    match (Control_flow_c.unwrap node1,Control_flow_c.unwrap node2) with
    | (Control_flow_c.SeqStart(_,_,_),Control_flow_c.SeqStart(_,_,_))
    | (Control_flow_c.SeqEnd(_,_),Control_flow_c.SeqEnd(_,_)) ->
      true
    | _ -> Cocci_addon.node_semantic_equality node1 node2
    end
  | _ -> false


let equal_nodes node1 node2 : bool =
  match (Control_flow_c.unwrap node1,Control_flow_c.unwrap node2) with
  | (Control_flow_c.SeqStart(_,_,_),Control_flow_c.SeqStart(_,_,_))
  | (Control_flow_c.SeqEnd(_,_),Control_flow_c.SeqEnd(_,_)) ->
    true
  | _ -> Cocci_addon.node_semantic_equality node1 node2


(**
    Improve quality of diff graph after it has been merged.
*)
let improve_diff_graph (diff_graph: diff_graph) : diff_graph =
    let common_prefix (key: int) =
        let all_edges =
            KeyEdgeSet.union
                (diff_graph#predecessors key)
                (diff_graph#successors key)
        in
        KeyEdgeSet.fold (fun (_, new_prefix) accu ->
            match accu with
            | `Unknown -> `Common(new_prefix)
            | `Common(old_prefix) when old_prefix = new_prefix -> accu
            | _ -> `NoCommon
        ) all_edges `Unknown
    in
    (* Some abstract nodes like FallThrough are not correctly marked.
        Hopefully due to the merging algorithm edges to these nodes are correct.
        If all edges share a common prefix then the node must have the same
        prefix.
    *)
    CFC.KeyMap.iter (fun key (old_prefix, node) ->
        match common_prefix key with
        | `Common(new_prefix) ->
            if (new_prefix <> old_prefix)
            then
                diff_graph#replace_node (key, (new_prefix, node))
        | `NoCommon | `Unknown -> ()
    ) diff_graph#nodes;
    diff_graph




(*
 * This is the main CFG-Diff creation function.
 * After tagging nodes as removed or added in the before, respectively after,
 * CFGs it merges the two CFGs into a CFG-Diff.
 *)
let merge_diff_graphs (index: int) (before: diff_graph) (after: diff_graph)
                      (line_map: int -> int) : (diff_graph * bool) =
    let str_index = (string_of_int index) ^  "_" in
    let end_name =
        (Filename.basename (fst !Debug.data.Debug.current_pair_of_files)) ^ "_" ^
        (Filename.basename (snd !Debug.data.Debug.current_pair_of_files)) ^
        "_spinfer.gv"
    in
    let nodes_before = before#nodes in
    let nodes_after = after#nodes in

    let node_has_prefix (prefix: prefix) ((_, node): (CEG.key * diff_node))
                        : bool =
        let node_prefix, _ = node in
        node_prefix = prefix
    in
    let nodes_removed =
        List.filter (node_has_prefix Before) (CFC.KeyMap.bindings nodes_before)
    in
    let nodes_unchanged_before =
        List.filter (node_has_prefix Both) (CFC.KeyMap.bindings nodes_before)
    in
    let nodes_unchanged_after =
        List.filter (node_has_prefix Both) (CFC.KeyMap.bindings nodes_after)
    in
    let nodes_added =
        List.filter (node_has_prefix After) (CFC.KeyMap.bindings nodes_after)
    in

    (* Unique nodes are special nodes that indicate CFGs start and end points *)
    let nodes_after_unique = List.filter (fun (_, (_, node)) ->
        match CFC.unwrap node with
        | CFC.TopNode | CFC.EndNode
        | CFC.Enter | CFC.Exit | CFC.ErrorExit -> true
        | _ -> false
    ) (CFC.KeyMap.bindings nodes_after)
    in
    let nodes_else_branch_removed = List.filter (fun (_, (_, node)) ->
        match CFC.unwrap node with
        | CFC.FallThroughNode | CFC.TrueNode(_) -> true
        | _ -> false
    ) nodes_unchanged_after
    in

    (* Non printable nodes does not have corresponding lines, but when they
    are created they are always associated with a printable nodes.
    A non-printable node and its associated printable node have the same labels
    by construction. *)
    let get_lines_from_same_labels (labels: int list) (nodes: diff_node list)
                                   : Diff.position =
        let rec find = function
        | [] ->
            Printf.eprintf "No line for labels: %s\n%!" (Dumper.dump labels);
            raise CFG.NoLineFound
        | head::tail ->
            try CFG.get_line_from_node (snd head)
            with CFG.NoLineFound -> find tail
        in
        let same_labels = List.filter (fun node ->
            CFC.extract_labels (snd node) = labels
        ) nodes
        in
        find same_labels
    in

    let get_line_from_node ((_, node): diff_node) (nodes: diff_node list)
                           : int * int =
        try
            match CFG.get_line_from_node node with
            | Diff.Range(l_begin, l_end) -> (l_begin, l_end)
            | _ -> assert false;
        with CFG.NoLineFound ->
            let labels = CFC.extract_labels node in
            match get_lines_from_same_labels labels nodes with
            | Diff.Range(l_begin, l_end) -> (l_begin, l_end)
            | _ -> assert false;
    in
    let remap (b, e) : int * int = (line_map b, line_map e) in

    (* Mapping from added labels to cfgdiff labels *)
    let labels_htbl = Hashtbl.create 100 in
    let before_rev_labels = List.map (fun (_, (_, node)) ->
        List.rev (Control_flow_c.extract_labels node)
    ) (CFC.KeyMap.bindings nodes_before)
    in
    let find_free_label node_labels =
        if Hashtbl.mem labels_htbl node_labels
        then
            Hashtbl.find labels_htbl node_labels
        else
            let rev_labels = List.rev node_labels in
            let rec new_rev_labels old =
                if old = []
                then
                    new_rev_labels [0]
                else if List.mem old before_rev_labels
                then
                    new_rev_labels (((List.hd old) + 1)::(List.tl old))
                else
                    old
            in
            let new_labels = List.rev (new_rev_labels rev_labels) in
            Hashtbl.add labels_htbl node_labels new_labels;
            new_labels
    in

    (* Satisfying labels uniqueness *)
    let nodes_added = List.map (fun (key, (prefix, ((node2, info), nodestr))) ->
        let new_info = Control_flow_c.{
            info with labels = find_free_label info.labels
        }
        in
        (key, (prefix, ((node2, new_info), nodestr)))
    ) nodes_added
    in

    (* Sometimes the function is totally new but share the same name as
     * a deleted function. We drop those cases *)
    let same_name_but_new_function =
        (* Contains new nodes *)
        nodes_added <> []
        &&
        (* Unchanged nodes are not real nodes *)
        nodes_unchanged_after = nodes_after_unique
    in

    if (nodes_removed <> [] || nodes_added <> []) && (not same_name_but_new_function)
    then
        let merged_graph = new CEG.ograph_mutable in
        CFC.KeyMap.iter (fun key node ->
            merged_graph#add_nodei key node
        ) nodes_before;
        let node_added_mapping = List.map (fun (key, node) ->
            (key, merged_graph#add_node node)
        ) nodes_added
        in
        CFC.KeyMap.iter (fun key _ ->
            KeyEdgeSet.iter (fun (succ_key, _) ->
                merged_graph#add_arc ((key, succ_key), Before)
            ) (before#successors key);
        ) nodes_before;

        (* Take an after key and return the corresponding key in merged graph *)
        let get_new_key key =
            if List.mem_assoc key node_added_mapping
            then
                (* First case node is added *)
                List.assoc key node_added_mapping
            else if List.mem_assoc key nodes_after_unique
            then
                (* Second case node is unique *)
                let after_node = List.assoc key nodes_unchanged_after in
                let new_key, _ = List.find (fun (_, before_node) ->
                    equal_nodes (snd before_node) (snd after_node)
                ) nodes_unchanged_before
                in
                new_key
            else
                (* Third case node is unchanged but not unique *)
                let after_node = List.assoc key nodes_unchanged_after in
                let lines_after =
                    get_line_from_node
                        after_node
                        (Utils.assoc_right nodes_unchanged_after)
                in
                try
                    let (new_key, _) = List.find (fun (_, before_node) ->
                        if equal_nodes (snd before_node) (snd after_node)
                        then
                            let lines_before =
                                get_line_from_node
                                    before_node
                                    (Utils.assoc_right nodes_unchanged_before)
                            in
                            lines_before = (remap lines_after)
                        else false
                    ) nodes_unchanged_before
                    in
                    new_key
                with
                | Not_found
                    when List.mem_assoc key nodes_else_branch_removed -> -1
        in


        (* Merging algorithm, at this stage we have all nodes added, removed or
        unmodified but only edges from the before CFG in our CFG-Diff.
        * The goal now is to add edges from the after CFG in our CFG-Diff.
        *)
        try
        CFC.KeyMap.iter (fun key _ ->
            let new_key = get_new_key key in
            KeyEdgeSet.iter (fun (succ_key, _) ->
                let new_succ_key = get_new_key succ_key in
                let before_edge = (new_succ_key, Before) in
                if new_key <> -1 && new_succ_key <> -1 then
                if KeyEdgeSet.mem before_edge (merged_graph#successors new_key)
                then begin
                    (* If edge was both in Before and After CFGs than it is unmodified *)
                    merged_graph#del_arc ((new_key, new_succ_key), Before);
                    merged_graph#add_arc ((new_key, new_succ_key), Both);
                end
                else
                    merged_graph#add_arc ((new_key, new_succ_key), After);
            ) (after#successors key);
        ) nodes_after;

        let merged_graph = improve_diff_graph merged_graph in
        (* Printing *)
        Debug.exec_if_graphs_dir
            (print_diff_graph merged_graph)
            (str_index ^ end_name);
        Debug.exec_if_graphs_dir
            (print_diff_graph before)
            (str_index ^ "before_" ^ end_name);
        Debug.exec_if_graphs_dir
            (print_diff_graph after)
            (str_index ^ "after_" ^ end_name);
        (merged_graph, true)
        with
        | Not_found ->
            Debug.exec_if_graphs_dir
                (print_diff_graph before)
                ("last_before_" ^ end_name);
            Debug.exec_if_graphs_dir
                (print_diff_graph after)
                ("last_after_" ^ end_name);
            Debug.exec_if_graphs_dir
                (print_diff_graph merged_graph)
                ("last_merged_" ^ end_name);
            Printf.eprintf "Error building: %s\n%!" end_name;
            Printexc.print_backtrace stderr;
            (before, false)
        | e ->
            Debug.exec_if_graphs_dir
                (print_diff_graph before)
                ("last_before_" ^ end_name);
            Debug.exec_if_graphs_dir
                (print_diff_graph after)
                ("last_after_" ^ end_name);
            raise e
    else
        (before, false)


let pair_functions (program_before: diff_graph list) (program_after: diff_graph list)
        : (diff_graph * diff_graph) list =
    (* We operate only on functions for now *)
    let is_header node : bool = match node with
    | Control_flow_c.FunHeader(_) -> true
    | _ -> false
    in
    (* Extract function (or struct ?) name and type of node *)
    let graph_to_fst_name_and_node graph : (string * CFC.node2)=
        let node2 = fst (fst (snd (CFC.KeyMap.find 1 graph#nodes))) in
        (Cocci_addon.stringify_CFG_node2 node2, node2)
    in
    let name_node_before = List.map graph_to_fst_name_and_node program_before in
    let name_node_after = List.map graph_to_fst_name_and_node program_after in
    let compare_fst a b = compare (fst a) (fst b) in

    (* Pairs functions from before and after files together *)
    let function_pairs =
        Utils.bilist_pairs (fun (_, (name1, node1)) (_, (name2, node2)) ->
            is_header node1 && is_header node2 && name1 = name2
        ) compare_fst name_node_before name_node_after
    in
    List.map (fun (index_before, index_after) ->
        let before = List.nth program_before index_before in
        let after = List.nth program_after index_after in
        before, after
    ) function_pairs

let merge_diff ((program_before, (filename_before, _)) : diff_program)
               ((program_after, (filename_after, _)) : diff_program)
               (index: int)
               (diffs: Diff.t list)
               : numbered_diff_program option * int =
    Debug.data := {!Debug.data with
        Debug.current_pair_of_files = (filename_before,filename_after)
    };
    Debug.print (Printf.sprintf "[INFO] Build diff CFG between %s and %s."
        (fst !Debug.data.Debug.current_pair_of_files)
        (snd !Debug.data.Debug.current_pair_of_files)
    );

    let line_map = Diff.diff_list_to_mapping diffs in

    let index = ref index in
    let diff_program =
        let accu = ref [] in
        List.iter (fun (before, after) ->
            let merged_graph, modified =
                merge_diff_graphs !index before after line_map
            in
            if modified
            then begin
                accu := (merged_graph, !index)::!accu;
                Hashtbl.add Global.id_to_filenames_htbl
                    !index (filename_before, filename_after);
                index := !index + 1;
            end
        ) (pair_functions program_before program_after);
        !accu
    in
    if (List.length diff_program > 0)
    then (Some(diff_program, (filename_before,filename_after)), !index)
    else (None, !index)


let fun_header_of_graph (graph: diff_graph) : Ast_c.definition option =
    let func_header = Control_flow_c.KeyMap.find 1 graph#nodes in
    match (fst (fst (snd func_header))) with
    | Control_flow_c.FunHeader(def) -> Some(def)
    | _ -> None