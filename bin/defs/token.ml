type token =
  (* values *)
  | Num of int
  | Name of string
  | True
  | False
  (* structures *)
  | Lparen
  | Rparen
  | Lbrack
  | Rbrack
  | Comma
  (* ordered comparison *)
  | Eq
  | Gt
  | Lt
  (* arithmetic *)
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  (* logic *)
  | And
  | Or
  | Not
  | Xor
  (* keywords *)
  | Let
  | Var
  | Print
  | Println
  | If
  | Then
  | Else
  | End
  | While
  | Do
  | Done

type t = token Loc.spanned
