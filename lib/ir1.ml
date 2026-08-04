type closure = (int, unit) Hashtbl.t

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
  (* function *)
  | FnVal of (int * Ast.mini_type) list * closure * Ast.mini_type * int * expr
  | FnCall of expr * expr list
  (* decs *)
  | Bind of int * expr
  | If of expr * expr * expr
  (* closure captures, body *)
  | Block of expr Array.t

and expr = e Loc.spanned
