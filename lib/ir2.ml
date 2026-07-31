type closure = unit Closure.t
type label = BREAK | CONT

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
  | Neq
  | Gt
  | Ge
  | Lt
  | Le
  (* lists *)
  | List of int
  | ListAt
  | StrAt
  (* function *)
  (* params, closure captures, body size *)
  | FnVal of string list * closure * int
  | FnCall of int
  (* decs *)
  | Store of string * int
  | Let of string
  (* skip if true *)
  | If
  | Jmp of int
  | JmpBck
  (* label *)
  | Label of label
