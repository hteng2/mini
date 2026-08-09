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
  | Semicolon
  (* types *)
  | To
  (* ordered comparison *)
  | Eq
  | Neq
  | Gt
  | Ge
  | Lt
  | Le
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
  | Bind
  | Print
  | Println
  | If
  | Else
  | Fn
  (**)
  | Import

type t = token Loc.spanned
