type expr =
  | Atom of int
  | Var of string
  | Assign of string * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

let combineOpts (l, r) =
  match l, r with
  | Some l, Some r -> Some (l, r)
  | _ -> None
;;

let rec analyze (ast : Parser.ast) =
  match ast with
  | Parser.Atom n -> Some (Atom n)
  | Parser.Var s -> Some (Var s)
  | Parser.Eq (l, r) ->
    (match combineOpts (analyze l, analyze r) with
     | Some (Var s, r) -> Some (Assign (s, r))
     | _ -> None)
  | Parser.Add (l, r) -> translate (l, r) (fun (l, r) -> Add (l, r))
  | Parser.Sub (l, r) -> translate (l, r) (fun (l, r) -> Sub (l, r))
  | Parser.Mul (l, r) -> translate (l, r) (fun (l, r) -> Mul (l, r))
  | Parser.Div (l, r) -> translate (l, r) (fun (l, r) -> Div (l, r))

and translate ((l, r) : Parser.ast * Parser.ast) (exprCons : expr * expr -> expr) =
  match combineOpts (analyze l, analyze r) with
  | Some (l, r) -> Some (exprCons (l, r))
  | _ -> None
;;

let print_expr expr =
  let rec p expr n =
    print_string (String.make (2 * n) ' ');
    match expr with
    | Atom n -> Printf.printf "Atom (%d)\n" n
    | Var n -> Printf.printf "Var (%s)\n" n
    | Assign (l, r) ->
      print_endline "Assign";
      p (Var l) (n + 1);
      p r (n + 1)
    | Add (l, r) ->
      print_endline "Add";
      p l (n + 1);
      p r (n + 1)
    | Sub (l, r) ->
      print_endline "Sub";
      p l (n + 1);
      p r (n + 1)
    | Mul (l, r) ->
      print_endline "Mul";
      p l (n + 1);
      p r (n + 1)
    | Div (l, r) ->
      print_endline "Div";
      p l (n + 1);
      p r (n + 1)
  in
  p expr 0
;;
