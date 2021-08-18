(**
   Usage of Unix diff command.
   @author Denis Merigoux
*)

(**{2 Type of diff output}*)

(** Type corresponding to diff standard output such as [3a4] or [12,14c15]. *)
type t = {
  before : position;
  operation : operation;
  after : position;
  source_file_before : string;
  source_file_after : string;
}

and position = Line of int
             | Range of int * int
and operation =  Added
              | Changed
              | Deleted

(**{2 Printing}*)

val stringify_position : position -> string
(**{2 Utilities}*)

val position_to_pair : position -> int * int

val diff_list_to_mapping : t list -> (int -> int)
(**
   [exec_diff (before,after)] executes [diff] on files [before] and [after] files
   and returns the formatted results.
*)
val exec_diff : (string*string) -> t list
val memoized_exec_diff : (string*string) -> t list
