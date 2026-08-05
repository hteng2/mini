type ir3 =
  (* atoms *)
  | Int of int
  | Float of float
  | Char of char
  | Name of int
  | Bool of bool
  | Void
  (* arith *)
  (* int *)
  | INeg
  | IAdd
  | ISub
  | IMul
  | IDiv
  | IMod
  (* float *)
  | FNeg
  | FAdd
  | FSub
  | FMul
  | FDiv
  (* logic *)
  | Not
  | And
  | Or
  | Xor
  (* comp *)
  (* int *)
  | IEq
  | INeq
  | IGt
  | IGe
  | ILt
  | ILe
  (* float *)
  | FGt
  | FGe
  | FLt
  | FLe
  (* char *)
  | CEq
  | CNeq
  | CGt
  | CGe
  | CLt
  | CLe
  (* bool *)
  | BEq
  (* lists *)
  | List of int
  | At
  (* function *)
  (* params, closure captures, body size *)
  | FnVal of int * int array * int
  | FnCall of int
  | FnTailCall of int
  (* decs *)
  | Bind of int
  (* skip if true *)
  | If
  | Jmp of int
  | JmpBck
  (* stack *)
  | Pop
