(**
   Various debug utilities and functions.
   @author Denis Merigoux
*)

(**
   Raised when a C++ declaration is encountered. The parsing library parse C
   and C++ so some C++ might be present.
*)
exception NotInC of string


(** Global variable to authorize to output the debug messages *)
let verbose = ref false

(** List of configuration variables *)

let diff_graphs_dir = ref ""
let patches_file = ref ""
let debug_file = ref ""
let unknown_bug_file = ref ""
let original_pwd = ref (Sys.getcwd ())
let ignored_files : string list ref = ref []
let tmp_dir = ref ""
let show_logs = ref false
let oracle = ref false

let make_absolute (path: string) : string =
    if Filename.is_relative path
    then !original_pwd ^ "/" ^ path
    else path


let static_dir =
    let executable_dir =
        let exec_path =
            try
                Unix.readlink Sys.executable_name
            with Unix.Unix_error _ -> Sys.executable_name
        in
        make_absolute (Filename.dirname exec_path)
    in
    let paths_to_try = [
        "../..";
        "../share/spinfer"
    ]
    in
    let abs_paths_to_try = List.map (fun rel ->
        executable_dir ^ "/" ^ rel ^ "/statics/"
    ) paths_to_try
    in
    try
        List.find (fun path ->
            Sys.file_exists path && Sys.is_directory path
        ) abs_paths_to_try
    with Not_found ->
        let msg = Printf.sprintf "Statics directory not found, tried: [%s]"
            (String.concat ", " abs_paths_to_try)
        in
        failwith msg

(**
  Useful data for debug everywhere in the program.
*)
type debug_env = {
  pairs_of_files : (string*string) list;
  current_pair_of_files : string * string ;
  current_file : string ;
}


let data = ref {
    pairs_of_files = [];
    current_pair_of_files = ("","");
    current_file = "";
  }

(**
  This function is used to print debug messages. The printing is enabled by the
  -v option.
*)
let print message =
  if (!verbose) then
    print_endline message

let create_tmp_directory () =
    let process_chan = Unix.open_process_in "mktemp -d /tmp/spinfer_XXXXXXXX" in
    tmp_dir := input_line process_chan;
    let status = Unix.close_process_in process_chan in
    match status with
    | Unix.WEXITED(0) -> ()
    | _ -> failwith "Failed to create tmp dir"

let get_tmp_directory () = !tmp_dir

let delete_tmp_directory () =
  ignore (Sys.command ("rm -rf " ^ !tmp_dir))


(**
    This function is used to generate graph output if --graphs-dir is passed
    in command line arguments
*)

let exec_if_graphs_dir (f: string -> unit) (name: string) : unit =
    if !diff_graphs_dir <> ""
    then begin
        let filename = !diff_graphs_dir ^ "/" ^ name in
        f filename
    end


(** Functions to print patch_debug *)
let patch_debug_file_chan = ref None
let patch_debug_file = ref ""

let rec print_patch_debug (str: string) : unit =
    match !patch_debug_file_chan with
    | Some(chan) ->
        output_string chan (str ^ "\n");
        flush chan
    | None ->
        if (!patch_debug_file <> "")
            then begin
                let chan = open_out !patch_debug_file in
                patch_debug_file_chan := Some(chan);
                print_patch_debug str
            end

(**
   Pair of  (list of flags to set true, list of flags to set false).
*)
let very_quiet_profile = (
  [
  ],
  [
    (* Flag_cocci.show_diff;   just leave this as it is *)

    Common.print_to_stderr;
    Flag.show_misc;
    Flag.show_trying;
    Flag.show_transinfo;

    Flag_cocci.show_c;
    Flag_cocci.show_cocci;
    Flag_cocci.show_flow;
    Flag_cocci.show_before_fixed_flow;
    Flag_cocci.show_ctl_tex;
    Flag_cocci.show_ctl_text;
    Flag_cocci.show_binding_in_out;

    Flag_cocci.verbose_cocci;

    Flag_parsing_c.show_parsing_error;

    Flag_parsing_c.verbose_lexing;
    Flag_parsing_c.verbose_parsing;
    Flag_parsing_c.verbose_type;
    Flag_parsing_c.verbose_cfg;
    Flag_parsing_c.verbose_unparsing;
    Flag_parsing_c.verbose_visit;
    Flag_parsing_c.verbose_cpp_ast;

    Flag_matcher.verbose_matcher;
    Flag_matcher.debug_engine;

    Flag_parsing_c.debug_unparsing;

    Flag_parsing_cocci.show_SP;
    Flag_parsing_cocci.show_iso_failures;

    Flag_ctl.verbose_ctl_engine;
    Flag_ctl.verbose_match;


  ])

(**
    Set the Coccinelle's flag according to a profile.

    @param profile Profile variable
  *)
let run_profile p =
  let (set_to_true, set_to_false) = p in
  List.iter (fun x -> x := false) set_to_false;
  List.iter (fun x -> x := true) set_to_true

(**
   Configure the "very quiet" mode of Coccinelle
   if the verbose option is not enabled, and the "debug" mode if it is enabled.
*)
let set_coccinelle_profile () =
  if (not !verbose) then
    run_profile very_quiet_profile

let previous_time = ref 0.
let accumulated_time = ref 0.
let timer_counter = ref 0
let start_timer () = previous_time := Unix.gettimeofday ()
let end_timer () =
    let new_time = Unix.gettimeofday () in
    incr timer_counter;
    accumulated_time := !accumulated_time +. (new_time -. !previous_time)
let reset_timer () = accumulated_time := 0.
let get_timer () = !accumulated_time
let get_timer_counter () = !timer_counter
