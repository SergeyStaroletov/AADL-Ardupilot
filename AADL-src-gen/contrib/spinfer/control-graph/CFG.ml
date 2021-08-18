(** @author Denis Merigoux *)

(**{2 Type definitions}*)

type node = Control_flow_c.node

type control_flow_graph = Control_flow_c.cflow

type program = control_flow_graph list * string

exception NoLineFound


let cfg_htbl = Hashtbl.create 25

(**{2 Building the CFG}*)
(**
   Performs some adjustments on the CFG to improve the usability of the CFG output
   by Coccinelle. Here is the list of the adjustments:
   - remove all [AfterNode].
*)
let adapt_CFG_from_cocci (cfg : control_flow_graph) : control_flow_graph =
  let first_key = Control_flow_c.first_node cfg in
  let nodes = cfg#nodes in
  let keys_to_delete = ref [] in
  let arcs_to_delete = ref [] in
  let mark_node key =
    let node = Control_flow_c.KeyMap.find key nodes in
    match Control_flow_c.unwrap node with
    | Control_flow_c.AfterNode(_) ->
      keys_to_delete := key::!keys_to_delete;
      let successors = cfg#successors key in
      let predecessors = cfg#predecessors key in
      Control_flow_c.KeyEdgeSet.iter
        (fun  (key',edge) -> arcs_to_delete := ((key,key'),edge)::!arcs_to_delete)
        successors;
      Control_flow_c.KeyEdgeSet.iter
        (fun  (key',edge) -> arcs_to_delete := ((key',key),edge)::!arcs_to_delete)
        predecessors;
    | _ -> ()
  in
  Control_flow_c.G.dfs_iter first_key mark_node cfg;
  List.iter (fun arc -> cfg#del_arc arc) !arcs_to_delete;
  List.iter (fun key -> cfg#del_node key) !keys_to_delete;
  cfg

(** Creates a graph filled with a [TopNode] and a [EndNode] linked together.*)
let new_empty_graph () : control_flow_graph =
  let new_graph = new Control_flow_c.G.ograph_mutable in
  new_graph#add_nodei 0 ((Control_flow_c.TopNode,{
      Control_flow_c.labels = [];
      Control_flow_c.bclabels = [];
      is_loop = false;
      is_fake = false;
    }),"");
  new_graph#add_nodei 1 ((Control_flow_c.EndNode,{
      Control_flow_c.labels = [];
      Control_flow_c.bclabels = [];
      is_loop = false;
      is_fake = false;
    }),"");
  new_graph#add_arc ((0,1),Control_flow_c.Direct);
  new_graph

let get_CFG_from_file (filename : string) : program =
  let (program, _) = Parse_c.parse_c_and_cpp false false filename in
  let (functions', _) = Common.unzip program in
  let functions'' = List.tl (List.rev functions') in
  Flag_parsing_c.verbose_type := false;
  let functions =
    List.map fst
      (Type_annoter_c.annotate_program !Type_annoter_c.initial_env functions'')
  in

  let aux toplevel =
    try begin
      match (Spinfer_control_flow_c_build.ast_to_control_flow toplevel) with
      | Some cfg -> adapt_CFG_from_cocci cfg
      | None -> new_empty_graph ()
    end with
    (**
       AST which yields an error on transformation to CFG are replaced by empty
       graphs.
    *)
    | Spinfer_control_flow_c_build.Error(error) ->
      new_empty_graph ()
  in

  let graphs = List.filter
        (fun g -> (Control_flow_c.KeyMap.cardinal g#nodes) > 2)
        (List.rev_map aux functions)
  in
  (graphs, filename)

let memoized_get_CFG_from_file (filename: string) : program =
    Utils.memoizer cfg_htbl filename (fun () -> get_CFG_from_file filename)

let speclist_to_CFGslist (speclist: Specfile.t) : ((program * program) list) =
    List.map (fun (before, after) ->
        (memoized_get_CFG_from_file before, memoized_get_CFG_from_file after)
    ) speclist

(**{2 Exploring the CFG}*)

let get_line_from_node (node : node) : Diff.position =
  try
    let (filename,func,(linbegin,colbegin),(linend,colend)) =
      Lib_parsing_c.lin_col_by_pos
        (Lib_parsing_c.ii_of_node node)
    in
    Diff.Range(linbegin,linend)
  with
  | Failure(_) -> raise NoLineFound
