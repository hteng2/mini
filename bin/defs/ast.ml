type mt =
  | MtBase of string
  | MtList of mini_type
  | MtFn of mini_type * mini_type list

and mini_type = mt Loc.spanned

type p = string * mini_type
and param = p Loc.spanned

type e =
  (* atoms *)
  | Num of int
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
  | FnVal of param list * mini_type * dec
  | FnCall of expr * expr list

and expr = e Loc.spanned
and id = IdName of string | IdAt of identifier * expr
and identifier = id Loc.spanned

and d =
  | Let of string * expr
  | Var of string * expr
  | VarSet of identifier * expr
  | Print of expr
  | Println of expr
  | If of expr * dec * dec option
  | While of expr * dec
  | Break
  | Continue
  | Block of program
  | Return of expr

and dec = d Loc.spanned
and program = dec Stream.t
