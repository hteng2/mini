(* IR-1 - SSA form *)

type rt =
  | RtBase of int
  | RtList of resolved_type
  | RtFn of resolved_type * resolved_type
  | RtTup of resolved_type list

and resolved_type = rt Loc.spanned

type param = PrmUnit | PrmLeaf of int * resolved_type | PrmTuple of param list
type pattern = PtrnUnit | PtrnLeaf of int | PtrnTuple of pattern list
type typedef = int * resolved_type

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
  (* type vars, param, closure, return type, self, symcnt, body*)
  | FnVal of int list * param * int list * resolved_type * int * int * expr
  | FnCall of expr * expr
  (* decs *)
  | Bind of pattern * expr
  | If of expr * expr * expr
  (* closure captures, body *)
  | Block of stmt Array.t * expr

and expr = e Loc.spanned
and stmt = Typedef of typedef | Expr of expr

type program = expr Array.t * int
