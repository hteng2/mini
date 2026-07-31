type mt =
  | MtBase of string
  | MtList of mini_type
  | MtFn of mini_type * mini_type list

and mini_type = mt Loc.spanned

type p = string * mini_type
and param = p Loc.spanned

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
  (* function *)
  | FnVal of param list * mini_type * expr
  | FnCall of expr * expr list
  (* decs *)
  | Let of string * expr
  | Var of string * expr
  | Set of expr * expr
  | If of expr * expr * expr
  | While of expr * expr
  | Break
  | Continue
  | Block of expr Queue.t

and expr = e Loc.spanned
