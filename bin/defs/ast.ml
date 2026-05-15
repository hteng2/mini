type e =
  | Num of int
  | Name of string
  | True
  | False
  | Neg of expr
  | Pos of expr
  | Not of expr
  | Eq of expr * expr
  | Gt of expr * expr
  | Lt of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Xor of expr * expr
  | Tuple of expr list

and expr = e Loc.spanned

type d =
  | Let of string * expr
  | Var of string * expr
  | VarSet of string * expr
  | Print of expr
  | If of if_stmt
  | While of expr * program

and if_stmt =
  | IfThen of expr * program
  | IfThenElse of expr * program * program

and dec = d Loc.spanned
and program = dec list
