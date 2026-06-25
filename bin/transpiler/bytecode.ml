type size = int
type data = Int of int | Bool of bool | Char of char

type code =
  (* stack/heap manipulation *)
  | Push of int
  | Pop
  | Fetch
  | Store
  | Copy of size
  (* control flow *)
  | Goto (* absolute *)
  | Call of int
  | Jump of size (* relative *)
  | Ifn (* skip next if true *)
  (* arithmetic *)
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  (* logic *)
  | And
  | Or
  | Xor
  (* comparison *)
  | Cmp
