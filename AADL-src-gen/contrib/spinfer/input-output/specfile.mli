(**
   Reading spinfer speficiation file.
   @author Denis Merigoux
*)

(**
   Type for the internal representation of speclist. Each item of the list is
   a pair (before,after) for the same C file before and after modification.
*)
type t = (string * string) list

(**
   [read_spec file] reads a specfile with name given in [file] into a list
   of pairs such that the first component is the name of the first
   file and the second component is the filename of the second
   component.
*)
val read_spec : string -> t
