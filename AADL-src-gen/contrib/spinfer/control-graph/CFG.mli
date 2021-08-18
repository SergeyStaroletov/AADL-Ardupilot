(**
   Functions related to handling Coccinelle's C control flow graphs.
   @author Denis Merigoux
*)

(** {2 Type definitions}
    The types are picked from Coccinelle's modules.
*)

(**
   An instruction node is a node of the CFG, modelled as an AST.
*)
type node = Control_flow_c.node

(**
   A control flow graph is a graph of instructions.
*)
type control_flow_graph = Control_flow_c.cflow

(**
   A program is a list of CFG, each corresponding to a function definition or
   global statement. The string is the name of the source code file corresponding
   to the program.
*)
type program = control_flow_graph list * string


(** Raised if one node doesn't contain any line information. *)
exception NoLineFound

(** {2 Building the CFG }*)

(**
   Use coccinelle Parse_c module to extract the CFG.
*)
val get_CFG_from_file : string -> program
val memoized_get_CFG_from_file : string -> program
(**
   Build the CFG of all the files listed in a [speclist], and output the CFG in
   pairs (before,after).
*)
val speclist_to_CFGslist : Specfile.t -> (program * program) list

(** {2 Exploring the CFG }*)

(**
  Retrieve the info attached to the expressions inside the nodes. This
  contains parsing info.
*)
val get_line_from_node : node -> Diff.position
