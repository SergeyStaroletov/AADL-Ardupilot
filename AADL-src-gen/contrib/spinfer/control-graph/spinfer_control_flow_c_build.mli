
val ast_to_control_flow : Ast_c.toplevel -> Control_flow_c.cflow option

type error =
  | DeadCode          of Common.parse_info option
  | CaseNoSwitch      of Common.parse_info
  | OnlyBreakInSwitch of Common.parse_info
  | WeirdSwitch       of Common.parse_info
  | NoEnclosingLoop   of Common.parse_info
  | GotoCantFindLabel of string * Common.parse_info
  | NoExit of Common.parse_info
  | DuplicatedLabel of string
  | NestedFunc
  | ComputedGoto
  | Define of Common.parse_info

exception Error of error

val mk_node : Control_flow_c.node2 -> int list -> int list -> string
  -> Control_flow_c.node

val then_start_str : string
val then_end_str : string
val fallthrough_start_str : string
val fallthrough_end_str : string
val else_start_str : string
val else_end_str : string
val else_node_str : string

val ifdef_then_start_str : string
val ifdef_then_end_str : string
val ifdef_fallthrough_start_str : string
val ifdef_fallthrough_end_str : string
val ifdef_else_start_str : string
val ifdef_else_end_str : string
val ifdef_else_node_str : string
