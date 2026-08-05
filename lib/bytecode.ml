type closure = (int, unit) Hashtbl.t

type e =
  (* atoms *)
  | Int of int
  | Float of float
  | Char of char
  | Str of string
  | Name of int
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
  | FnVal of int * int array * int
  | FnCall of int
  | FnTailCall of int
  (* decs *)
  | Bind of int
  (* skip if true *)
  | If
  | Jmp of int
  | JmpBck
  (**)
  | Pop
