type closure = unit Closure.t

type e =
  (* atoms *)
  | Num of int
  | Char of char
  | Str of string
  | Name of string
  | Bool of bool
  | Void
  (* arith *)
  | Neg
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  (* logic *)
  | Not
  | And
  | Or
  | Xor
  (* comp *)
  | Eq
  | Gt
  | Lt
  (* lists *)
  | List of int
  | ListAt
  | StrAt
  (* function *)
  (* params, closure captures, body *)
  | FnVal of string list * closure * dec
  | FnCall of int

and expr = e Array.t
and identifier = IdName of string | IdAt of identifier * expr

and dec =
  | Let of string * expr
  | Var of string * expr
  | VarSet of identifier * expr
  | If of expr * dec * dec option
  | While of expr * dec
  | Break
  | Continue
  (* closure captures, body *)
  | Block of dec Queue.t
  | Return of expr
