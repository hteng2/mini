type ast =
  | Empty
  | Num of int
  | Name of string
  | Neg of ast
  | Cmd of string
  | Assign of string * ast
  | Add of ast * ast
  | Sub of ast * ast
  | Mul of ast * ast
  | Div of ast * ast

let prefix_bp = 7

let prefix_combine (op, r) =
  match op with '-' -> Neg r | _ -> raise (Failure "unrecognized operator")

let bp (op : char) =
  match op with
  | '=' -> (2, 1)
  | '+' -> (3, 4)
  | '-' -> (3, 4)
  | '*' -> (5, 6)
  | '/' -> (5, 6)
  | _ -> raise (Failure "unrecognized operator")

let combine (l, op, r) =
  match op with
  | '=' -> (
      match l with
      | Name s -> Assign (s, r)
      | _ -> raise (Failure "expected name"))
  | '+' -> Add (l, r)
  | '-' -> Sub (l, r)
  | '*' -> Mul (l, r)
  | '/' -> Div (l, r)
  | _ -> raise (Failure "unrecognized operator")

let rec pratt (ts : Lexer.token list) (min_bp : int)
    (k : Lexer.token list * ast -> 'a) =
  match ts with
  | Lexer.Num n :: ts' -> advance ts' min_bp (Num n) k
  | Lexer.Name n :: ts' -> advance ts' min_bp (Name n) k
  | Lexer.Op op :: ts' ->
      pratt ts' prefix_bp (fun (ts'', inner) ->
          advance ts'' min_bp (prefix_combine (op, inner)) k)
  | Lexer.Colon :: ts' ->
      pratt ts' prefix_bp (fun (ts'', inner) ->
          match inner with
          | Name s -> advance ts'' min_bp (Cmd s) k
          | _ -> raise (Failure "expected name"))
  | Lparen :: ts' ->
      pratt ts' 0 (fun (ts'', inner) ->
          match ts'' with
          | Rparen :: ts''' -> advance ts''' min_bp inner k
          | _ -> raise (Failure "expected Rparen"))
  | [] -> Empty
  | _ -> raise (Failure "unexpected token")

and advance (ts : Lexer.token list) (min_bp : int) (left : ast)
    (k : Lexer.token list * ast -> 'a) =
  match ts with
  | [] -> k ([], left)
  | Lexer.Rparen :: _ -> k (ts, left)
  | Lexer.Op op :: ts' ->
      let l, r = bp op in
      if min_bp >= l then k (ts, left)
      else
        pratt ts' r (fun (ts'', right) ->
            advance ts'' min_bp (combine (left, op, right)) k)
  | _ -> raise (Failure "unexpected token")

let parse ts =
  pratt ts 0 (fun (ts', x) ->
      match ts' with
      | [] -> x
      | Lexer.Rparen :: _ -> raise (Failure "unexpected Rparen")
      | _ -> raise (Failure "unexpected remaining tokens"))

let print_ast ast =
  let rec p ast n =
    print_string (String.make (2 * n) ' ');
    match ast with
    | Empty -> Printf.printf "Empty\n"
    | Num n -> Printf.printf "Atom (%d)\n" n
    | Name n -> Printf.printf "Var (%s)\n" n
    | Cmd s -> Printf.printf "Cmd %s\n" s
    | Neg e ->
        print_endline "Neg";
        p e (n + 1)
    | Assign (s, r) ->
        Printf.printf "Assign %s\n" s;
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
