(* @author Sergey Staroletov. CFG/CFC code is from Spinfer project *)

open Common

let func_of_interest = ["rc_loop"  ;  "throttle_loop"; "update_GPS"; "update_batt_compass"; "arm_motors_check";
 "auto_trim"; "update_altitude"; "run_nav_updates"; "update_thr_cruise"; "three_hz_loop"; "compass_accumulate";
 "barometer_accumulate"; "update_notify"; "one_hz_loop"; "ekf_dcm_check"; "crash_check"; "gcs_check_input";
 "gcs_send_heartbeat"; "gcs_send_deferred"; "gcs_data_stream_send"; "update_mount"; "ten_hz_logging_loop"; 
 "fifty_hz_logging_loop"; "perf_update";"read_receiver_rssi"; "telemetry_send"  ] in

let intervals = [4;8;8;40;40;40;40;40;8;40;133;8;8;8;400;40;40;8;400;8;8;8;40;8;40;80;4;8;40;120;400] in
let max_times = [10;45;90;72;5;1;14;100;80;10;9;42;25;10;42;2;2;550;150;720;950;45;30;22;5;10;10;10;10;10;10] in


type control_flow_graph = Control_flow_c.cflow

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




let dir_is_empty dir =
  Array.length (Sys.readdir dir) = 0

(** [dir_contents] returns the paths of all regular files that are
 * contained in [dir]. Each file is a path starting with [dir].
  *)
let dir_contents dir =
  let rec loop result = function
    | f::fs when Sys.is_directory f ->
          Sys.readdir f
          |> Array.to_list
          |> List.map (Filename.concat f)
          |> List.append fs
          |> loop result
    | f::fs -> loop (f::result) fs
    | []    -> result
  in
    loop [] [dir]

let contains s1 s2 =
    let re = Str.regexp_string s2
    in
        try ignore (Str.search_forward re s1 0); true
        with Not_found -> false


let get_interesting_files ext1 ext2 dir = 
List.filter(
  fun x -> ((Filename.extension x = ext1) || (Filename.extension x = ext2)) && not (contains x "examples")
) (dir_contents dir)


let _ =

 let c_files = get_interesting_files ".pde" ".cpp" "/home/sergey/ardu_analize" in
 List.iter(
   fun f -> Printf.printf "[file] %s\n" f 
 ) c_files;

 (*let c_files = ["/home/sergey/ardu_analize/ArduCopter2.pde"; "/home/sergey/ardu_analize/switches.pde"] in*)


 
(*
1) obtain all cfg from files in dir and get a map function its cfg
2) for all funct of interest do:
 - prepare a list (set) of called functions from it
 - forall fun in the list do start its cfg analize
 - prepare variables in the left part
 - prepare variables in the right part
 - save map fun->(set,get vars)
 3) create a graph with relations based on this map
*)


let hash_fun2cfg = Hashtbl.create 101 in

Flag.c_plus_plus := true; (* magic *)


List.iter (
  fun f -> 

    Printf.printf "Scanning %s...\n" f;

    let (cfg, _) = CFG.get_CFG_from_file f in

    List.iteri (
      fun i x -> 

      let is_fun = ref false in
      let name_fun = ref "" in

      let first_key = Control_flow_c.first_node x in

      let nodes = x#nodes in
      let handle key = 
        let node = Control_flow_c.KeyMap.find key nodes in
        let handle_node (nod, s) = 
          let (node2, _) = nod in
          match node2 with
          Control_flow_c.FunHeader(def) -> (
            let def2 = Ast_c.unwrap def in
            match def2.f_name with
            Ast_c.RegularName n -> 
              name_fun:= Ast_c.unwrap n;
              is_fun:=true
            | _ -> ()
          )
          | _ -> ();

        if !is_fun then Printf.printf "[fun=%s] node %s\n" !name_fun s
        else Printf.printf "node %s\n" s;
        in
        handle_node node
      in
      Control_flow_c.G.dfs_iter first_key handle x;
      if !is_fun then Hashtbl.add hash_fun2cfg !name_fun x;
    Printf.printf "subgraph\n"

  ) cfg;

) c_files;



  Printf.printf "[HT] has %d elements\n" (Hashtbl.length hash_fun2cfg);

  let hash_fun2set = Hashtbl.create 101 in
  let hash_fun2get = Hashtbl.create 101 in

  List.iter(
    fun f -> 
      Printf.printf "Stage2 for fun %s...\n" f;
      let g = try Some(Hashtbl.find hash_fun2cfg f) with Not_found -> None  in 
      match g with 
      Some(fg) -> (
        let fg_to_visit = ref[fg] in 
        let fg_visited = ref[] in
        let var_set = ref[] in
        let var_get = ref[] in
        
        let rec looper fg_to_visit =

        Printf.printf "[looper call]\n";

        (*while ((List.length !fg_to_visit) > 0) &&  (List.length !fg_to_visit) <> (List.length !fg_visited) do*)
          
          let rest = List.filter (
            fun x -> not(List.mem x !fg_visited) 
          ) !fg_to_visit in

          let rest = List.hd rest in (* we get 1st element of unvisited *)

          (* rec expr hanlder *)

          let rec scan_expression e =
            let ((e,_),_) = e in
            match e with
              Ast_c.Binary(e1,op,e2) -> scan_expression e1; scan_expression e2
              | Ast_c.Unary(e1, op) -> scan_expression e1
              | Ast_c.Postfix(e1, op) -> scan_expression e1
              | Ast_c.Infix(e1, op) -> scan_expression e1
              | Ast_c.FunCall(ef,arg) -> ( 
                let ((ef,_),_) = ef in 
                match ef with 
                  Ast_c.Ident(Ast_c.RegularName nam)-> (
                    let nam = Ast_c.unwrap nam in
                    Printf.printf "[FUN! %s]\n" nam; 
                    (* add to visited *)
                    let g = try Some(Hashtbl.find hash_fun2cfg nam) with Not_found -> None in

                    match g with
                      Some(fg) -> if not (List.mem fg !fg_to_visit) then
                        begin
                          fg_to_visit:=!fg_to_visit @[fg];
                          Printf.printf "[fun] added fun %s to list\n" nam 
                        end
                      | _ -> ()
                  )
                  | _ -> ();
                  List.iter(
                    fun ar ->
                    Printf.printf "[param func]\n"; 
                    match Ast_c.unwrap2 ar with Left argu-> scan_expression argu
                    | _ -> ()
                  ) arg)
              | Ast_c.Ident(Ast_c.RegularName nam) -> 
                let nam = Ast_c.unwrap nam in
                if not(List.mem nam !var_get) then 
                  var_get := !var_get @ [nam];

                Printf.printf "[IDENT in the right part!!] %s\n" nam
               | _ -> ()
            in

          (* processing code *)
          let handle_node ((node2, nodeinfo), s) = 
            match node2 with
            Control_flow_c.ExprStatement(a,b) -> 
            (
              Printf.printf "[expr] \n" (*Dumper.dump b*);
              let (e, ebis) = b in
              match e with
                Some((Ast_c.Assignment (a1,a2,a3),_),_) -> (
                  Printf.printf "[assignment!]\n";
                  match a1 with
                  ((Ast_c.Ident(nam),_),_) -> 
                    let (nam, _) = Ast_c.get_s_and_ii_of_name nam in 
                    if not(List.mem nam !var_set) then 
                      var_set := !var_set @ [nam];
                    Printf.printf "NAM = %s\n" nam
                  | _ -> ();
                  scan_expression a3
                )
              | Some((Ast_c.FunCall(ef,arg),_),_) -> ( (* fun call not in the assignment *)
                Printf.printf "fun call\n";
                let ((ef,_),_) = ef in 
                match ef with 
                  Ast_c.Ident(Ast_c.RegularName nam)-> (
                    let nam = Ast_c.unwrap nam in
                    Printf.printf "[FUN! %s]\n" nam; 
                    (* add to visited *)
                    let g = try Some(Hashtbl.find hash_fun2cfg nam) with Not_found -> None in

                    match g with
                      Some(fg) -> 
                      if not (List.mem fg !fg_to_visit) then
                        begin
                          fg_to_visit:=!fg_to_visit @[fg];
                          Printf.printf "[fun] added fun %s to list\n" nam 
                        end
                      | _ -> ()
                  )
                  | _ -> ();
                  List.iter(
                   fun ar ->
                    Printf.printf "[param1 func]\n"; 
                    match Ast_c.unwrap2 ar with Left argu-> scan_expression argu
                    | _ -> ()
                  ) arg)
              | _-> () 
            ) 
            | _-> ()
            in
  
          (* iteration code *)
          let first_key = try Some(Control_flow_c.first_node rest) with Not_found -> None in (**fg *)
          let nodes = rest#nodes in
          let handle key = 
            let node = Control_flow_c.KeyMap.find key nodes in
            handle_node node
          in

          match first_key with 
          Some(fk) -> (
            Control_flow_c.G.dfs_iter fk handle rest; (* we iterate on a non-visited *)
            fg_visited := !fg_visited @[rest];
            Printf.printf "Checking condition len = %d\n" (List.length !fg_to_visit);
            if  ((List.length !fg_to_visit) > 0) && (List.length !fg_to_visit) <> (List.length !fg_visited) then
              looper fg_to_visit
            )
          | None -> (Printf.printf "!! No FK\n");


         

        (*done;*)

        in
        looper fg_to_visit;
        Printf.printf "[looper]\n";

        Hashtbl.add hash_fun2get f !var_get;
        Hashtbl.add hash_fun2set f !var_set

      )
      |None -> Printf.printf "!!!! func %s not found in cfgs \n" f;

  ) func_of_interest;

Printf.printf "\n\n[RESULT]\n";

List.iter (
fun k -> 
  Printf.printf "Func %s\n" k;
  let var_set = try Hashtbl.find hash_fun2set k with Not_found -> [] in
  List.iter(
    fun x -> Printf.printf " [it sets] %s\n" x
  ) var_set;
  let var_get = try Hashtbl.find hash_fun2get k with Not_found -> [] in
  List.iter(
    fun x -> Printf.printf " [it gets] %s\n" x
  ) var_get
) func_of_interest;


Printf.printf "\n=====[Distinct data]====\n\n\n";

let hash_fun2set2 = Hashtbl.create 101 in
let hash_fun2get2 = Hashtbl.create 101 in

let l = List.length func_of_interest in
let links = Array.make_matrix l l false in

List.iteri (
fun i k -> 
  let var_set = try Hashtbl.find hash_fun2set k with Not_found -> [] in
  let vars_ok = ref [] in
  List.iter(
    fun x -> 
    begin
      List.iteri (
        fun ii kk -> 
          if kk <> k then
          begin 
            let var_get = try Hashtbl.find hash_fun2get kk with Not_found -> [] in
            if (List.mem x var_get) && not(List.mem x !vars_ok) then 
              begin
                vars_ok:= !vars_ok @ [x];
                links.(i).(ii) <- true
              end
          end
      ) func_of_interest
    end
  ) var_set;
  Hashtbl.add hash_fun2set2 k !vars_ok;

  let var_get = try Hashtbl.find hash_fun2get k with Not_found -> [] in
  
  let vars_ok = ref [] in
  List.iter(
    fun x -> 
    begin
      List.iteri (
        fun ii kk -> 
          if kk <> k then
          begin 
            let var_set = try Hashtbl.find hash_fun2set kk with Not_found -> [] in
            if (List.mem x var_set) && not(List.mem x !vars_ok)  then 
              begin
                vars_ok:= !vars_ok @ [x];
                links.(i).(ii) <- true
              end
          end
      ) func_of_interest
    end
  ) var_get;
  Hashtbl.add hash_fun2get2 k !vars_ok;
) func_of_interest;



List.iter (
fun k -> 
  Printf.printf "Func %s\n" k;
  let var_set = try Hashtbl.find hash_fun2set2 k with Not_found -> [] in
  List.iter(
    fun x -> Printf.printf " [it sets] %s\n" x
  ) var_set;
  let var_get = try Hashtbl.find hash_fun2get2 k with Not_found -> [] in
  List.iter(
    fun x -> Printf.printf " [it gets] %s\n" x
  ) var_get
) func_of_interest;


  Printf.printf "=======\n\n\nAADL generated:\n\n\n";

  Printf.printf "package apm_system \n";
  Printf.printf "public \n";
	
	Printf.printf "system APM_2_6 \n";
	Printf.printf "end APM_2_6; \n";



  List.iteri (
    fun i fn ->
    Printf.printf " \n";
    Printf.printf "-- %s \n" fn;
    Printf.printf "\n";
 	  Printf.printf "thread thr_%s \n" fn;
   	Printf.printf "properties \n";
    Printf.printf "  Dispatch_Protocol => Periodic; \n";
    Printf.printf "  Compute_Execution_Time => 0 ms .. %d ms; \n" (List.nth max_times i);
    Printf.printf "  Period => %d Ms; -- 1000 / (100 / x) = 10 * x = 10 * %d \n" (10*(List.nth intervals i)) (List.nth intervals i);
    Printf.printf "end thr_%s; \n" fn;
   	Printf.printf "thread implementation thr_%s.i \n" fn;
   	Printf.printf "end thr_%s.i; \n" fn;
    Printf.printf " \n";
	  Printf.printf "process process_%s \n" fn;
	  Printf.printf "end process_%s; \n" fn;
    Printf.printf " \n";
	  Printf.printf "process implementation process_%s.i  \n" fn;
		Printf.printf "subcomponents \n";
		Printf.printf "thr: thread thr_%s.i; \n" fn;
	  Printf.printf "end process_%s.i; \n\n" fn
  ) func_of_interest;

Printf.printf "\nsystem implementation APM_2_6.i \n";
Printf.printf "subcomponents \n";

List.iteri  (
    fun i fn ->
   Printf.printf "prs_%s        : process   process_%s.i; \n" fn fn;
) func_of_interest;

Printf.printf "connections \n";
Printf.printf "end APM_2_6.i; \n";
Printf.printf " \n";
Printf.printf "end apm_system; \n"