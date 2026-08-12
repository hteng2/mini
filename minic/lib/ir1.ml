(* IR-1 - SSA form *)
type param = PrmUnit | PrmLeaf of int * Ast.mini_type | PrmTuple of param list
type pattern = PtrnUnit | PtrnLeaf of int | PtrnTuple of pattern list

type e =
  (* atoms *)
  | Int of int
  | Float of float
  | Char of char
  | Str of string
  | Name of int
  | Bool of bool
  | Void
  (* arith *)
  | Neg of expr
  | Pos of expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  (* logic *)
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Xor of expr * expr
  (* comp *)
  | Eq of expr * expr
  | Neq of expr * expr
  | Gt of expr * expr
  | Ge of expr * expr
  | Lt of expr * expr
  | Le of expr * expr
  (* lists *)
  | List of expr list
  | At of expr * expr
  (* tuple *)
  | Tuple of expr list
  (* function *)
  (* param, closure, return type, self, symcnt, body*)
  | FnVal of param * int list * Ast.mini_type * int * int * expr
  | FnCall of expr * expr
  (* decs *)
  | Bind of pattern * expr
  | If of expr * expr * expr
  (* closure captures, body *)
  | Block of expr Array.t

and expr = e Loc.spanned

type program = expr Array.t * int
