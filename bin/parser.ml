type ast =
  | Atom of int
  | Var of string
  | Eq of ast * ast
  | Add of ast * ast
  | Sub of ast * ast
  | Mul of ast * ast
  | Div of ast * ast

let bp (op : Lexer.token) =
  match op with
  | Lexer.Op '=' -> Some (2, 1)
  | Lexer.Op '+' -> Some (3, 4)
  | Lexer.Op '-' -> Some (3, 4)
  | Lexer.Op '*' -> Some (5, 6)
  | Lexer.Op '/' -> Some (5, 6)
  | _ -> None
;;

let combine (l, op, r) =
  match op with
  | Lexer.Op '=' -> Some (Eq (l, r))
  | Lexer.Op '+' -> Some (Add (l, r))
  | Lexer.Op '-' -> Some (Sub (l, r))
  | Lexer.Op '*' -> Some (Mul (l, r))
  | Lexer.Op '/' -> Some (Div (l, r))
  | _ -> None
;;

let rec pratt ts min_bp k =
  match ts with
  | Lexer.Num n :: ts' -> advance ts' min_bp (Atom n) k
  | Lexer.Name n :: ts' -> advance ts' min_bp (Var n) k
  | _ ->
    print_endline "c";
    k ([], None)

and advance ts min_bp left k =
  match ts with
  | [] -> k ([], Some left)
  | (Lexer.Op c as t) :: ts' ->
    (match bp t with
     | None ->
       print_endline "c";
       k (ts', None)
     | Some (l, r) ->
       if min_bp >= l
       then k (ts, Some left)
       else
         pratt ts' r (fun (ts'', opt_right) ->
           match opt_right with
           | None ->
             print_endline "b";
             k (ts'', None)
           | Some right ->
             (match combine (left, t, right) with
              | None ->
                print_endline "a";
                k (ts'', None)
              | Some left' -> advance ts'' min_bp left' k)))
  | _ ->
    print_endline "0";
    k ([], None)
;;

let parse ts = pratt ts 0

let print_ast ast =
  let rec p ast n =
    print_string (String.make (2 * n) ' ');
    match ast with
    | Atom n -> Printf.printf "Atom (%d)\n" n
    | Var n -> Printf.printf "Var (%s)\n" n
    | Eq (l, r) ->
      print_endline "Eq";
      p l (n + 1);
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
  p ast 0
;;
