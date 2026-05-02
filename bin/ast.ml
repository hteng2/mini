type expr =
  | Num of int
  | Name of string
  | True
  | False
  | Neg of expr
  | Pos of expr
  | Not of expr
  | Eq of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Xor of expr * expr

type 'a dec = Let of string * 'a | Print of 'a | If of 'a * 'a dec list
