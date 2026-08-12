type ir3 =
  (* atoms *)
  | Int of int
  | Float of float
  | Char of char
  | Name of int
  | Bool of bool
  | Void
  | Pop
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
  | FLt
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
  (* tuples *)
  | Tuple of int
  | Destruct
  (* function *)
  (* closure captures, sym count, body size *)
  | FnVal of int array * int * int
  | FnCall
  | FnTailCall
  (* decs *)
  | Bind of int
  (* skip if true *)
  | If
  | Jmp of int
  | JmpBck

type program = ir3 Array.t * int
