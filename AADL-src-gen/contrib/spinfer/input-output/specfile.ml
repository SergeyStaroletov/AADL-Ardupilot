(** @author Denis Merigoux *)

type t = (string * string) list

let read_spec (file : string) : t =
  (** Separator for files is at least one space (not tabs) *)
  let filesep = Str.regexp " +" in
  (*Debug.print ("[INFO] Specfile is: " ^ file^".");*)
  let file_pairs = ref [] in
  let ins = open_in file in
  let rec loop () =
    let next_line = input_line ins in
    let next_two  = Str.split filesep next_line in
    if List.length next_two > 1
    then (*failwith "index file lines must contain two file names");*)
    if Str.string_before (List.hd next_two) 1 = "#"
    then
      Debug.print "# Comment"
    else (
      (*Debug.print ("[INFO] Pair of files: " ^
                   List.nth next_two 0 ^ ", " ^
                   List.nth next_two 1^".");*)
      file_pairs := (
        List.nth next_two 0,
        List.nth next_two 1) ::
        !file_pairs);
    loop ()
  in
  try loop () with
    End_of_file ->
      close_in ins;
      !file_pairs
