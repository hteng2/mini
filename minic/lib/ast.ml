type pt =
  | PtBase of string
  | PtList of parsed_type
  | PtFn of parsed_type * parsed_type
  | PtTup of parsed_type list

and parsed_type = pt Loc.spanned

type prm = PrmUnit | PrmLeaf of string * parsed_type | PrmTuple of param list
and param = prm Loc.spanned

type pattern = PtrnUnit | PtrnLeaf of string | PtrnTuple of pattern list
type typedef = string * parsed_type

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
  | FnVal of string list * param * parsed_type * expr
  | FnCall of expr * expr
  (* decs *)
  | Bind of pattern * expr
  | If of expr * expr * expr
  | Block of stmt Queue.t * expr

and expr = e Loc.spanned
and stmt = Typedef of typedef | Expr of expr
