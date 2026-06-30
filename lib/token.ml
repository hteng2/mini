type token =
  (* values *)
  | Int of int
  | Float of float
  | Str of string
  | Char of char
  | Name of string
  | True
  | False
  (* structures *)
  | Lparen
  | Rparen
  | Lbrack
  | Rbrack
  | Lbrace
  | Rbrace
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
  | Else
  | While
  | Break
  | Continue
  | Fn
  | Return
  (**)
  | Import

type t = token Loc.spanned
