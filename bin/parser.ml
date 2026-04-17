type ast =
  | Empty
  | Num of int
  | Name of string
  | Assign of ast * ast
  | Add of ast * ast
  | Sub of ast * ast
  | Mul of ast * ast
  | Div of ast * ast

let bp (op : Lexer.token) =
  match op with
  | Lexer.Op '=' -> (2, 1)
  | Lexer.Op '+' -> (3, 4)
  | Lexer.Op '-' -> (3, 4)
  | Lexer.Op '*' -> (5, 6)
  | Lexer.Op '/' -> (5, 6)
  | _ -> raise (Failure "unrecognized operator")

let combine (l, op, r) =
  match op with
  | Lexer.Op '=' -> Assign (l, r)
  | Lexer.Op '+' -> Add (l, r)
  | Lexer.Op '-' -> Sub (l, r)
  | Lexer.Op '*' -> Mul (l, r)
  | Lexer.Op '/' -> Div (l, r)
  | _ -> raise (Failure "unrecognized operator")

let rec pratt (ts : Lexer.token list) (min_bp : int)
    (k : Lexer.token list * ast -> 'a) =
  match ts with
  | Lexer.Num n :: ts' -> advance ts' min_bp (Num n) k
  | Lexer.Name n :: ts' -> advance ts' min_bp (Name n) k
  | [] -> Empty
  | _ -> raise (Failure "expected atom")

and advance ts min_bp left (k : Lexer.token list * ast -> 'a) =
  match ts with
  | [] -> k ([], left)
  | (Lexer.Op _ as t) :: ts' ->
      let l, r = bp t in
      if min_bp >= l then k (ts, left)
      else
        pratt ts' r (fun (ts'', right) ->
            let left' = combine (left, t, right) in
            advance ts'' min_bp left' k)
  | _ -> raise (Failure "unexpected token")

let parse ts = pratt ts 0 (fun (_, x) -> x)

let print_ast ast =
  let rec p ast n =
    print_string (String.make (2 * n) ' ');
    match ast with
    | Empty -> Printf.printf "Empty\n"
    | Num n -> Printf.printf "Atom (%d)\n" n
    | Name n -> Printf.printf "Var (%s)\n" n
    | Assign (l, r) ->
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
