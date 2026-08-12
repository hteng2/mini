type mt =
  | MtBase of string
  | MtList of mini_type
  | MtFn of mini_type * mini_type
  | MtTup of mini_type list

and mini_type = mt Loc.spanned

type prm = PrmUnit | PrmLeaf of string * mini_type | PrmTuple of param list
and param = prm Loc.spanned

type pattern = PtrnUnit | PtrnLeaf of string | PtrnTuple of pattern list

type e =
  (* atoms *)
  | Int of int
  | Float of float
  | Char of char
  | Str of string
  | Name of string
  | True
  | False
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
  | Gt of expr * expr
  | Lt of expr * expr
  | Neq of expr * expr
  | Ge of expr * expr
  | Le of expr * expr
  (* lists *)
  | List of expr list
  | At of expr * expr
  (* tuples *)
  | Tuple of expr list
  (* function *)
  | FnVal of param * mini_type * expr
  | FnCall of expr * expr
  (* decs *)
  | Bind of pattern * expr
  | If of expr * expr * expr
  | Block of expr Queue.t

and expr = e Loc.spanned
