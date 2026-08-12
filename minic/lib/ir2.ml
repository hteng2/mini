(* IR-2 - Typed SSA form *)
type pattern = PtrnUnit | PtrnLeaf of int | PtrnTuple of pattern list

type e =
  (* atoms *)
  | Int of int
  | Float of float
  | Char of char
  | Name of int
  | Bool of bool
  | Void
  (* arith *)
  (* int *)
  | INeg of expr
  | IAdd of expr * expr
  | ISub of expr * expr
  | IMul of expr * expr
  | IDiv of expr * expr
  | IMod of expr * expr
  (* float *)
  | FNeg of expr
  | FAdd of expr * expr
  | FSub of expr * expr
  | FMul of expr * expr
  | FDiv of expr * expr
  (* logic *)
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Xor of expr * expr
  (* comp *)
  (* int *)
  | IEq of expr * expr
  | INeq of expr * expr
  | IGt of expr * expr
  | IGe of expr * expr
  | ILt of expr * expr
  | ILe of expr * expr
  (* float *)
  | FGt of expr * expr
  | FLt of expr * expr
  (* char *)
  | CEq of expr * expr
  | CNeq of expr * expr
  | CGt of expr * expr
  | CGe of expr * expr
  | CLt of expr * expr
  | CLe of expr * expr
  (* bool *)
  | BEq of expr * expr
  (* lists *)
  | List of expr array
  | At of expr * expr
  (* tuple *)
  | Tuple of expr list
  (* function *)
  (* closure, symcnt, body *)
  | FnVal of pattern * int list * int * expr
  | FnCall of expr * expr
  | FnTailCall of expr * expr
  (* decs *)
  | Bind of pattern * expr
  | If of expr * expr * expr
  (* closure captures, body *)
  | Block of expr Array.t
  (* etc *)
  | Do of expr
  | Noop

and expr = e Loc.spanned

type program = expr Array.t * int
