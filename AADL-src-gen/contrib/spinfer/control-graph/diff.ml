(** @author Denis Merigoux *)

(**{2 Type of diff output}*)

type t = {
  before : position;
  operation : operation;
  after : position;
  source_file_before : string;
  source_file_after : string;
}

and position = Line of int
             | Range of int * int
and operation =  Added   (** 0 *)
              | Changed  (** 1 *)
              | Deleted  (** 2 *)

(**{2 Printing}*)

let diff_htbl = Hashtbl.create 30
let stringify_position (position : position) : string =
  match position with
  | Line(i) -> Printf.sprintf "%d" i
  | Range(i,j) -> Printf.sprintf "%d,%d" i j

(**{2 Utilities}*)
let position_to_pair (position: position) : int * int =
    match position with
    | Range(i, j) -> (i, j)
    | Line(i) -> (i, i)


let position_to_last (position: position) : int =
    snd (position_to_pair position)

let diff_list_to_mapping (diffs: t list) : int -> int =
    let positions = List.map (fun diff ->
        (position_to_last diff.after, position_to_last diff.before)
    ) diffs
    in
    let ordered_positions = List.sort compare positions in
    let rec find_until line_after last = function
    | [] -> last
    | head::tail ->
        if (fst head >= line_after)
        then last
        else find_until line_after head tail
    in
    let get_line_before (line_after: int) : int =
        let last_diff = find_until line_after (0, 0) ordered_positions in
        let offset = line_after - (fst last_diff) in
        assert (offset >= 0);
        let line_before = (snd last_diff) + offset in
        line_before
    in
    get_line_before

(**{2 Running the diff}*)

(**
   Parses a string like ["7,8"] into [Range(7,8)] and "6" into [Line(6)].
*)
let parse_position (position : string) : position =
  let delim = Str.regexp "," in
  let split = Str.split delim position in
  try
    begin match split with
      | [first';last'] ->
        let first = int_of_string first' in
        let last = int_of_string last' in
        Range(first,last)
      | [line'] ->
        let line = int_of_string line' in
        Line(line)
      | _ -> failwith (Printf.sprintf "Wrong format for diff output : %s"
                         position)
    end
  with
  | Failure(_) -> failwith (Printf.sprintf "Wrong format for diff output : %s"
                              position)


(**
   Parses a string like ["7,8c6"] into [(Range(7,8),Changed,Line(6))].
*)
let parse_diff_action
    (source_before : string)
    (source_after : string)
    (action' : string )
  : t =
  let action_line_separator = Str.regexp "a\\|d\\|c" in
  let action = Str.full_split action_line_separator action' in
  begin match action with
    | [Str.Text(before');
       Str.Delim(a_or_c_or_d);
       Str.Text(after')] ->
      let op = begin match a_or_c_or_d with
        | "a" -> Added
        | "c" -> Changed
        | "d" -> Deleted
        | _ ->
          failwith (Printf.sprintf "Wrong format for diff output : %s"
                      action')
      end in
      let before = parse_position before' in
      let after = parse_position after' in
      {
        before = before;
        after = after;
        operation = op;
        source_file_before = source_before;
        source_file_after = source_after;
      }
    | _ ->
      failwith (Printf.sprintf "Wrong format for diff output : %s" action')
  end

(**
   Counts how many lines one have to skip in a diff output file to get to the
   next diff action depending on the last diff action read. For instance,
   [(Range(7,8),Changed,Line(6))] will give two lines removed and one line
   added, with a --- between : that makes 4 lines to skip.
*)
let count_lines_to_skip (diff : t) : int =
  let position_line_count position = match position with
    | Range(first,last) -> last-first +1
    | Line(_) -> 1
  in
  match diff.operation with
  | Added -> position_line_count diff.after
  | Deleted -> position_line_count diff.before
  | Changed ->
    (position_line_count diff.before) + (position_line_count diff.after) + 1

let read_diff
    (ins : in_channel)
    (source_before : string)
    (source_after : string)
  : t list =
  let diff_list = ref [] in
  let rec loop skip_count  =
    if (skip_count = 0) then
      let next_line = input_line ins in
      let diff = parse_diff_action source_before source_after next_line in
      diff_list := diff::!diff_list;
      let new_skip_count = count_lines_to_skip diff in
      loop new_skip_count
    else
      let new_skip_count = skip_count - 1 in
      ignore (input_line ins);
      loop new_skip_count
  in
  try loop 0 with
    End_of_file ->
        ignore (Unix.close_process_in ins);
        !diff_list

(**
   Execute the diff command on the two files in argument and returns diff output
   as defined in the {!type:Diff.t} type.
*)
let exec_diff ((before, after) : string * string) : t list =
  Debug.data := { !Debug.data with Debug.current_pair_of_files = (before,after)};
  let command =
    Printf.sprintf "diff -w --speed-large-files %s %s" before after
  in
  let chan = Unix.open_process_in command in
  let result = read_diff chan before after in
  result

let memoized_exec_diff (couple: string * string) :  t list =
    Utils.memoizer diff_htbl couple (fun () -> exec_diff couple)