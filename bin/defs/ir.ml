type closure = unit Closure.t

type expr =
  (* atoms *)
  | Num of int
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
  (* lists *)
  | List of expr list
  | At of expr * expr
  (* function *)
  (* params, closure captures, body *)
  | FnVal of string list * closure * dec
  | FnCall of expr * expr list

and identifier = IdName of string | IdAt of identifier * expr

and dec =
  | Let of string * expr
  | Var of string * expr
  | VarSet of identifier * expr
  | Print of expr
  | Println of expr
  | If of expr * dec * dec option
  | While of expr * dec
  | Break
  | Continue
  (* closure captures, body *)
  | Block of program
  | Return of expr

and program = dec Stream.t
