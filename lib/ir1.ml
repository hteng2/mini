type closure = unit Closure.t

type e =
  (* atoms *)
  | Int of int
  | Float of float
  | Char of char
  | Str of string
  | Name of string
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
  | Gt of expr * expr
  | Lt of expr * expr
  (* lists *)
  | List of expr list
  | ListAt of expr * expr
  | StrAt of expr * expr
  (* function *)
  | FnVal of string list * closure * dec
  | FnCall of expr * expr list

and expr = e * Types.tt
and identifier = IdName of string | IdAt of identifier * expr

and d =
  | Let of string * expr
  | Var of string * expr
  | VarSet of identifier * expr
  | If of expr * dec * dec option
  | While of expr * dec
  | Break
  | Continue
  (* closure captures, body *)
  | Block of dec Array.t
  | Return of expr

and dec = d Loc.spanned
