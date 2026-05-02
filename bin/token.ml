type token =
  (* values *)
  | Num of int
  | Name of string
  | True
  | False
  (* structures *)
  | Lparen
  | Rparen
  (* operators *)
  | Eq
  | Gt
  | Lt
  | Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Not
  | Xor
  (* keywords *)
  | Let
  | Print
  | If
  | Then
  | End
