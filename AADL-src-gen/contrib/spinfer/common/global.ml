let id_to_filenames_htbl = Hashtbl.create 50

let id_to_filenames (id: int) : (string * string) =
    Hashtbl.find id_to_filenames_htbl id

let added_idents : (int, string) Hashtbl.t = Hashtbl.create 50
let in_decl_ident : (int, string) Hashtbl.t = Hashtbl.create 50
let in_func_ident : (int, string) Hashtbl.t = Hashtbl.create 50
let context_idents : (int, string) Hashtbl.t = Hashtbl.create 50

let specfile = ref ""
let specfile_test = ref ""
let specfile_final_test = ref ""
let headerfiles = ref ""
let testheaderfiles = ref ""
let infer_dir = ref ""
let test_dir = ref ""
let final_test_dir = ref ""
let progress = ref true
let no_single_match = ref false
let feasible_metrics = ref false
let spatch_path = ref (Debug.static_dir ^ "spatch")
let iso_file = ref (Debug.static_dir ^ "standard.iso")
let builtins_file = ref (Debug.static_dir ^ "standard.h")

let only_encode = ref ""

let files_uid_htbl = Hashtbl.create 50
let uid_files_htbl = Hashtbl.create 50

let register_files (files: string * string) : int =
    if Hashtbl.mem files_uid_htbl files
    then
        Hashtbl.find files_uid_htbl files
    else
        let next_uid = Hashtbl.length files_uid_htbl in
        Hashtbl.add files_uid_htbl files next_uid;
        Hashtbl.add uid_files_htbl next_uid files;
        next_uid

let metacount = ref 0
let fresh_metavar () : string =
    let result = !metacount in
    incr metacount;
    "X" ^ string_of_int result

let error_patch_counter = ref 0
