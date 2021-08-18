(**
   List of  carried by tree, corresponding to C AST types.
   @author Denis Merigoux
*)

type t =
  (** Internal nodes *)
  | If (** [if(cond)], 1 child. *)
  | Switch (** [switch(cond)], 1 child. *)
  | Case (** [case (expr)], 1 child. *)
  | IfDef (** [#ifdef(cond)], 1 child. *)
  | While (** [while(cond)], 1 child. *)
  | For (** [for(x;y;z)], 3 children. *)
  | Goto (** [goto foo], 1 child. *)
  | Label (** [foo:], 1 child. *)
  | CaseRange (** [case 0..10:], 2 children. *)
  | Return (** [return foo;], 0 or 1 child. *)
  | FunctionDefinition (** [static int foo(int,bar)], 4 children. *)
  | Attributes (** [static public], as many children as attributes. *)
  | FunctionCall (** [foo(bar,code)], two chidren. *)
  | Arguments (** [(foo,bar)], as many children as arguments. *)
  | DeclarationList (** As many children as declarations. *)
  | VariableDeclaration (** [type foo (= bar);], 3 or 2 children. *)
  | MacroDeclaration (** [FOO(bar,code) = foo], 3 children. *)
  | MacroCall (** Idem as [FunctionCall]. *)
  | TernaryCondition (** [x ? y : z], 3 children. *)
  | Assignment (** [a+=b], 3 or 4 children. *)
  | Postfix (** [i++], 2 children. *)
  | Infix (** [++i], 2 children. *)
  | Unary (** [- x], 2 children. *)
  | Binary (** [x+y], 3 children. *)
  | ArrayAccess (** [array[i]], 2 children. *)
  | RecordAccess (** [object.field], 2 children. *)
  | RecordPointerAccess (** [object->field], 2 children. *)
  | Object (** [object(->field)], 1 child. *)
  | Field (** [(object)->field], 1 child*)
  | SizeOfType (** [sizeof(foo)], 1 child. *)
  | SizeOfExpr (** [sizeof(foo)], 1 child. *)
  | Cast (** [(int)foo], 2 children. *)
  | Parenthesis (** [(foo)], 1 child. *)
  | Sequence (** [instr1;instr2], two children. *)
  | Identifier
  | Constant (** Any type of number, one child. *)

  (** Leaves *)
  | Value of string
  | Type of Ast_c.fullType (** Any type declaration. *)
  | Else (** [else] *)
  | Do (** [do] *)
  | Continue (** [continue] *)
  | Break (** [break] *)
  | Default (** [default] *)
  | GetRef (** [&foo] *)
  | DeRef (** [*foo] *)
  | Assign (** [=] *)
  | Decrement (** [--] *)
  | Increment (** [++] *)
  | Plus (** [+] *)
  | Minus (** [-] *)
  | Multiply (** [*] *)
  | Divide (** [/] *)
  | Modulo (** [%] *)
  | ShiftLeft (** [<<] *)
  | ShiftRight (** [>>] *)
  | Not (** [!x] *)
  | And (** [&&] *)
  | Or (** [||] *)
  | BitwiseAnd (** [&] *)
  | BitwiseOr (** [|] *)
  | BitwiseNot (** [~x] *)
  | Xor (** [^] *)
  | InferiorTo (** [<] *)
  | SuperiorTo (** [>] *)
  | InferiorOrEqualTo (** [<=] *)
  | SuperiorOrEqualTo (** [>=] *)
  | EqualTo (** [==] *)
  | NotEqualTo (** [!==] *)
  | Empty

(**{2 Auxiliary functions}*)

let stringify_label (label : t) : string =
  match label with
  | If -> "If"
  | Switch -> "Switch"
  | Case -> "Case"
  | While -> "While"
  | For -> "For"
  | Goto -> "Goto"
  | Label -> "Label"
  | CaseRange -> "CaseRange"
  | Return -> "Return"
  | FunctionDefinition -> "FunctionDefinition"
  | Attributes -> "Attributes"
  | FunctionCall -> "FunctionCall"
  | Arguments -> "Arguments"
  | DeclarationList -> "DeclarationList"
  | VariableDeclaration -> "VariableDeclaration"
  | MacroDeclaration -> "MacroDeclaration"
  | MacroCall -> "MacroCall"
  | TernaryCondition -> "TernaryCondition"
  | Assignment -> "Assignment"
  | Postfix -> "Postfix"
  | Infix -> "Infix"
  | Unary -> "Unary"
  | Binary -> "Binary"
  | ArrayAccess -> "ArrayAccess"
  | RecordAccess -> "RecordAccess"
  | RecordPointerAccess -> "RecordPointerAccess"
  | Object -> "Object"
  | Field -> "Field"
  | SizeOfType -> "SizeOfType"
  | SizeOfExpr -> "SizeOfExpr"
  | Cast -> "Cast"
  | Parenthesis -> "Parenthesis"
  | Sequence -> "Sequence"
  | Else -> "Else"
  | Do -> "Do"
  | Continue -> "Continue"
  | Break -> "Break"
  | Default -> "Default"
  | GetRef -> "GetRef"
  | DeRef -> "DeRef"
  | Assign -> "Assign"
  | Decrement -> "Decrement"
  | Increment -> "Increment"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Multiply -> "Multiply"
  | Divide -> "Divide"
  | Modulo -> "Modulo"
  | ShiftLeft -> "ShiftLeft"
  | ShiftRight -> "ShiftRight"
  | Not -> "Not"
  | And -> "And"
  | Or -> "Or"
  | BitwiseAnd -> "BitwiseAnd"
  | BitwiseOr -> "BitwiseOr"
  | BitwiseNot -> "BitwiseNot"
  | Xor -> "Xor"
  | InferiorTo -> "InferiorTo"
  | SuperiorTo -> "SuperiorTo"
  | InferiorOrEqualTo -> "InferiorOrEqualTo"
  | SuperiorOrEqualTo -> "SuperiorOrEqualTo"
  | EqualTo -> "EqualTo"
  | NotEqualTo -> "NotEqualTo"
  | Empty -> "Empty"
  | IfDef -> "IfDef"
  (**| Value(str) -> "\""^str^"\""*)
  | Value(str) -> str
  | Identifier -> "Identifier"
  | Constant -> "Constant"
  | Type(typ) -> "Type("^(Cocci_addon.stringify_fullType typ)^")"


(**
   Compares two labels, ignoring the difference of values in [Identifier],
   [Constant] and [Type].
*)
let abstract_equality (label1 : t) (label2 : t) : bool =
  match (label1, label2) with
  | (Value(_),Value(_)) -> true
  | _ -> if (label1 = label2)
    then true
    else false
