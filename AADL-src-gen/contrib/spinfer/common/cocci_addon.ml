(**
   Functions related to Coccinelle's objects.
   @author Denis Merigoux
*)

(** {2 Printing functions} *)

(**
   Various printing functions for Coccinelle's objects. The functions raise
   {!exception:Debug.NotInC} exception when given to print non-C constructions.
*)

module F = Control_flow_c

let inner_buffer = Buffer.create 80
let formatter = Format.formatter_of_buffer inner_buffer

let pr_space _ = Format.pp_print_space formatter ()

let pr_elem info =
  let s = Ast_c.str_of_info info in
  Format.pp_print_string formatter s

let pr_nl _ = ()
let pr_indent _ = ()
let pr_outdent _ = ()
let pr_unindent _ = ()


let ppc =
  Pretty_print_c.mk_pretty_printers
    ~pr_elem ~pr_space ~pr_nl ~pr_outdent ~pr_indent ~pr_unindent

let pp_param = ppc.Pretty_print_c.param
let pp_expression = ppc.Pretty_print_c.expression
let pp_name = ppc.Pretty_print_c.name
let pp_type_ident = ppc.Pretty_print_c.type_with_ident
let pp_type = ppc.Pretty_print_c.ty
let pp_decl = ppc.Pretty_print_c.decl
let pp_statement = ppc.Pretty_print_c.statement
let pp_base_type = ppc.Pretty_print_c.base_type
let pp_type_ident_rest = ppc.Pretty_print_c.type_with_ident_rest


let inline_format pretty_printer arg =
  Format.pp_open_hbox formatter ();
  pretty_printer arg;
  Format.pp_close_box formatter ();
  Format.pp_print_flush formatter ();
  let inlined_str = Buffer.contents inner_buffer in
  Buffer.clear inner_buffer;
  inlined_str

let inline_format2 pretty_printer arg1 arg2 =
  Format.pp_open_hbox formatter ();
  pretty_printer arg1 arg2;
  Format.pp_close_box formatter ();
  Format.pp_print_flush formatter ();
  let inlined_str = Buffer.contents inner_buffer in
  Buffer.clear inner_buffer;
  inlined_str

let stringify_param param =
  inline_format pp_param param

let stringify_name (name : Ast_c.name) : string =
  inline_format pp_name name

let stringify_fullType (typ : Ast_c.fullType) : string =
  inline_format pp_type typ

let stringify_base_type (typ : Ast_c.fullType) : string =
  inline_format pp_base_type typ

let stringify_type_with_ident (typ : Ast_c.fullType)  (ident : string)
                              : string =
  (* Stringify type with a name
   Usefull for array and function which type
   *can't* be splitted as: `type name`
   *)
  let ident_print = fun _ -> Format.pp_print_string formatter ident in
  inline_format2 pp_type_ident typ ident_print

let stringify_type_with_ident_rest (typ : Ast_c.fullType)  (ident : string)
                              : string =
  let ident_print = fun _ -> Format.pp_print_string formatter ident in
  inline_format2 pp_type_ident_rest typ ident_print

let stringify_declaration (decl' : Ast_c.declaration) : string =
  inline_format pp_decl decl'

let stringify_statement (statement : Ast_c.statement) : string =
  inline_format pp_statement statement

let stringify_expression (expr : Ast_c.expression) : string =
  inline_format pp_expression expr


let rec stringify_attributes (attrs : Ast_c.attribute list) : string =
  match attrs with
  | [] -> ""
  | (Ast_c.Attribute(attr),_)::rest -> attr^" "^(stringify_attributes rest)

(**
   As functionType contains both the return type and the parameters, the output
   is not one contiguous string to allow C-Style printing. The output is
   (return type, parameters).
*)
let stringify_functionType (functype : Ast_c.functionType) : (string*string) =
  let (typ, (args,_)) = functype in
  let strtyp = stringify_fullType typ in
  let rec stringify_args args =
    match args with
    | [] -> ""
    | [(paramtyp,_)] ->
      stringify_param paramtyp
    | (paramtyp,_)::rest ->
      let strparam = stringify_param paramtyp in
      strparam^", "^(stringify_args rest)
  in
  let strargs = stringify_args args in
  (strtyp,"("^strargs^")")

let stringify_definition ((def,_) : Ast_c.definition) : string =
  let strname = stringify_name def.Ast_c.f_name in
  let strattrs = stringify_attributes def.Ast_c.f_attr in
  let (strtyp,strargs) = stringify_functionType def.Ast_c.f_type in
  let res = strattrs^" "^strtyp^" "^strname^strargs in
  res

let stringify_CFG_node2 (node2 : Control_flow_c.node2 ) : string =
  match node2 with
  | Control_flow_c.TopNode -> "Top"
  | Control_flow_c.EndNode -> "Bottom"

  | Control_flow_c.FunHeader(def) -> stringify_definition def
  | Control_flow_c.Decl(decl) -> stringify_declaration decl

  | Control_flow_c.SeqStart(_,d,_) -> Printf.sprintf "{ (%d)" d
  | Control_flow_c.SeqEnd(d,_) -> Printf.sprintf "} (%d)" d

  | Control_flow_c.ExprStatement(state,(expr',_)) ->
    begin match expr' with
      | Some(expr) -> (stringify_expression expr) ^ " ;"
      | None -> (stringify_statement state)
    end

  | Control_flow_c.IfHeader(state,(expr,_)) ->
    "if ("^(stringify_expression expr)^")"
  | Control_flow_c.Else(_) -> "else"
  | Control_flow_c.WhileHeader(stmt,(expr,_)) ->
    "while ("^(stringify_expression expr)^")"
  | Control_flow_c.DoHeader(_,_) ->
    "do"
  | Control_flow_c.DoWhileTail(expr,_) ->
    "while ("^(stringify_expression expr)^")"
  | Control_flow_c.ForHeader(_,((init,(ending,_),(incr,_)),_)) ->
    let strinit = begin match init with
      | Ast_c.ForDecl(decl) -> stringify_declaration decl
      | Ast_c.ForExp((Some expr ,_)) -> stringify_expression expr
      | Ast_c.ForExp((None,_)) -> ""
    end in
    let strincr = begin match incr with
      | Some(expr) -> stringify_expression expr
      | None -> ""
    end in
    let strending = begin match ending with
      | Some(expr) -> stringify_expression expr
      | None -> ""
    end in
    "for ("^strinit^";"^strending^";"^strincr^")"
  | Control_flow_c.SwitchHeader(_,(expr,_)) ->
    "switch ("^(stringify_expression expr)^")"

  | Control_flow_c.EndStatement(_) -> "end"

  | Control_flow_c.Return(stmt,_) ->
    stringify_statement stmt
  | Control_flow_c.ReturnExpr(stmt,_) ->
    stringify_statement stmt

  | Control_flow_c.DefineHeader((name,_),_) ->
    "#define "^name^" = "
  | Control_flow_c.DefineExpr(expr) ->
    stringify_expression expr
  | Control_flow_c.DefineType(typ) ->
    stringify_fullType typ
  | Control_flow_c.DefineDoWhileZeroHeader(_) -> "/DefineDoWhileZeroHeader/"
  | Control_flow_c.DefineTodo -> "/DefineTodo/"

  | Control_flow_c.Include(_) -> "#include /Something/"
  | Control_flow_c.PragmaHeader(_,_) -> "#pragma"

  | Control_flow_c.MacroTop(_,_,_) -> "#macro"

  | Control_flow_c.Case(stmt,(expr,_)) ->
    let strexpr = stringify_expression expr in
    "case: "^strexpr
  | Control_flow_c.Default(_,_) ->
    "default :"
  | Control_flow_c.Continue(_,_) ->
    "continue;"
  | Control_flow_c.Break(_,_,_) ->
    "break;"
  | Control_flow_c.CaseRange(_,((e1,e2),_)) ->
    let stre1 = stringify_expression e1 in
    let stre2 = stringify_expression e2 in
    "case "^stre1^" ... "^stre2^":"
  | Control_flow_c.Label(_,name,_) ->
    let strname = stringify_name name in
    strname^":"
  | Control_flow_c.Goto(_,name,_) ->
    let strname = stringify_name name in
    "goto: "^strname^";"

  | Control_flow_c.MacroStmt(stmt,_) ->
    stringify_statement stmt

  | Control_flow_c.Enter -> "/Enter/"
  | Control_flow_c.Exit -> "/Exit/"
  | Control_flow_c.Fake -> "/Fake/"
  | Control_flow_c.CaseNode(i) -> Printf.sprintf "/CaseNode(%d)/" i

  | Control_flow_c.TrueNode(boolref) -> Printf.sprintf "/TrueNode(%b)/" !boolref
  | Control_flow_c.FalseNode -> "/FalseNode/"
  | Control_flow_c.InLoopNode -> "/InLoopNode/"
  | Control_flow_c.AfterNode(after) -> begin match after with
      | Control_flow_c.RetAfterNode -> "/RetAfterNode/"
      | Control_flow_c.GotoAfterNode -> "/GotoAfterNode/"
      | Control_flow_c.BreakAfterNode -> "/BreakAfterNode/"
      | Control_flow_c.ContAfterNode -> "/ContAfterNode/"
      | Control_flow_c.SWBreakAfterNode -> "/SWBreakAfterNode/"
      | Control_flow_c.NormalAfterNode -> "/NormalAfterNode/"
    end
  | Control_flow_c.FallThroughNode -> "/FallThroughNode/"
  | Control_flow_c.LoopFallThroughNode -> "/LoopFallThroughNode/"
  | Control_flow_c.ErrorExit -> "/ErrorExit/"
  | Control_flow_c.IfdefHeader _ -> "/IfdefHeader/"
  | Control_flow_c.IfdefElse _ -> "/IfdefElse/"
  | Control_flow_c.IfdefEndif _ -> "/IfdefEndif/"
  | Control_flow_c.MacroIterHeader (_, ((name, _), _)) ->
      Printf.sprintf "/MacroIter(%s)/" name

  | _ -> raise (Debug.NotInC "Unknown control flow node.")

let stringify_CFG_node1 (node1 : Control_flow_c.node1) : string =
  match node1 with
  | (node2, nodeinfo) ->
  (stringify_CFG_node2 node2)^" (Labels: "^
                       (Dumper.dump nodeinfo.Control_flow_c.labels) ^ ")"

(**
     Printer for the main node object.
*)
let stringify_CFG_node (node : Control_flow_c.node) : string =
  try
    match node with
    | (node1, str) -> stringify_CFG_node1 node1
  with
  | Debug.NotInC(error) -> Printf.sprintf "/Unknown node : %s/" error

  (**{2 Testing equality }*)

let fake_statement = (Ast_c.MacroStmt,[])

let real_al_node n =
  let n = Lib_parsing_c.real_al_node n in
  (* we don't want wrapping, ie the info and nodestring, which contain
     labels that are sensitive to the position in the CFG *)
  match F.unwrap n with
    | F.ExprStatement(st, (eopt, ii)) ->
	F.ExprStatement(fake_statement, (eopt, ii))
    | F.IfHeader(st, (e,ii)) ->
        F.IfHeader(fake_statement, (e,ii))
    | F.SwitchHeader (st, (e,ii)) ->
        F.SwitchHeader(fake_statement, (e,ii))
    | F.WhileHeader(st, (e,ii))  ->
        F.WhileHeader(fake_statement, (e,ii))
    | F.ForHeader(st, ((first, (e2opt,i2), (e3opt,i3)), ii)) ->
	F.ForHeader(fake_statement, ((first, (e2opt,i2), (e3opt,i3)), ii))
    | F.MacroIterHeader(st, ((s,es), ii)) ->
	F.MacroIterHeader(fake_statement, ((s,es), ii))
    | F.ReturnExpr(st, (e,ii)) ->
	F.ReturnExpr(fake_statement, (e,ii))
    | F.Case(st, (e,ii)) ->
	F.Case(fake_statement, (e,ii))
    | F.CaseRange(st, ((e1, e2),ii)) ->
	F.CaseRange (fake_statement, ((e1, e2),ii))
    | F.MacroStmt(st, ((),ii)) ->
	F.MacroStmt(fake_statement, ((),ii))
    | F.Asm (st, (body,ii)) ->
	F.Asm(fake_statement, (body,ii))
    | F.Exec(st, (code,ii)) ->
        F.Exec(fake_statement, (code,ii))
    | F.Break    (st,((),ii),fromswitch) ->
        F.Break(fake_statement,((),ii),fromswitch)
    | F.Continue (st,((),ii)) ->
	F.Continue(fake_statement,((),ii))
    | F.Default  (st,((),ii)) ->
	F.Default(fake_statement,((),ii))
    | F.Return   (st,((),ii)) ->
	F.Return(fake_statement,((),ii))
    | F.Goto  (st, name, ((),ii)) ->
        F.Goto(fake_statement, name, ((),ii))
    | F.Label (st, name, ((),ii)) ->
        F.Label(fake_statement, name, ((),ii))
    | F.DoHeader (st, info) ->
	F.DoHeader(fake_statement, info)
    | F.SeqStart (st, i, info) ->
	F.SeqStart(fake_statement, 0, info)
    | F.SeqEnd (_, info) ->
	F.SeqEnd(0, info)

    | (
        (
          F.TopNode|F.EndNode|
          F.ErrorExit|F.Exit|F.Enter|F.LoopFallThroughNode|F.FallThroughNode|
          F.AfterNode _|F.FalseNode|F.TrueNode _|F.InLoopNode|
          F.Fake|F.FunHeader _|F.Decl _|F.DoWhileTail _|F.CaseNode _|
	  F.DefineHeader _|F.DefineExpr _|F.DefineType _|
	  F.DefineDoWhileZeroHeader _|F.DefineTodo|F.PragmaHeader _|
	  F.Include _|F.Else _|F.IfdefHeader _| F.DefineInit _ |
	  F.IfdefElse _|F.IfdefEndif _|F.IfdefIteHeader _|F.MacroTop _|
	  F.EndStatement _
        ) as x) -> x

let node_semantic_equality n1 n2 =
  real_al_node n1 = real_al_node n2
  &&
  (* Testing string equality *)
  (snd n1) = (snd n2)

let rec extract_typedef (fullType: Ast_c.fullType) : string option =
  (* Extract the typedef name from a complete type.
   * Useful for crafting semantic patches because coccinelle need to know
   * if a type is a typedef in the semantic patch header.
   *)
  let typeCbis = (fun (_, (typeCbis, _)) -> typeCbis) fullType in
  let findtype = function
  | Ast_c.TypeName(name, _) -> Some(stringify_name name)
  | Ast_c.Pointer(fullType)
  | Ast_c.Array(_, fullType)
  | Ast_c.FieldType(fullType, _, _)
  | Ast_c.ParenType(fullType)
  | Ast_c.TypeOfType(fullType) -> extract_typedef fullType
  | _-> None
  in
  findtype typeCbis
