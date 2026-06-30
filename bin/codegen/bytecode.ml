type code =
  (* stack/heap manipulation *)
  | Push of int
  | Alloc
  | Store
  | Fetch
  (* control flow *)
  | Goto
  | Call of int
  | Jump of int
  | Ifn
  (* arithmetic *)
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
  (* comparison *)
  | Eq
  | Gt
  | Lt
