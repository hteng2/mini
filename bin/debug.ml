let print_tokens ts =
  let t2s t =
    match t with
    | Token.Num n -> Printf.sprintf "Num %d\t" n
    | Token.Name n -> Printf.sprintf "Name %s\t" n
    | Token.True -> "True\t"
    | Token.False -> "False\t"
    | Token.Lparen -> "Lparen\t"
    | Token.Rparen -> "Rparen\t"
    | Token.Lbrack -> "Lbrack\t"
    | Token.Rbrack -> "Rbrack\t"
    | Token.Comma -> "Comma\t"
    | Token.Eq -> "Eq\t"
    | Token.Gt -> "Gt\t"
    | Token.Lt -> "Lt\t"
    | Token.Add -> "Add\t"
    | Token.Sub -> "Sub\t"
    | Token.Mul -> "Mul\t"
    | Token.Div -> "Div\t"
    | Token.Mod -> "Mod\t"
    | Token.And -> "And\t"
    | Token.Or -> "Or\t"
    | Token.Not -> "Not\t"
    | Token.Xor -> "Xor\t"
    | Token.Let -> "Let\t"
    | Token.Var -> "Var\t"
    | Token.Print -> "Print\t"
    | Token.Println -> "Println\t"
    | Token.If -> "If\t"
    | Token.Then -> "Then\t"
    | Token.Else -> "Else\t"
    | Token.End -> "End\t"
    | Token.While -> "While\t"
    | Token.Do -> "Do\t"
    | Token.Done -> "Done\t"
  in
  List.fold_left
    (fun _ ->
      fun ({ v; span = (sr, sc), (er, ec) } : Token.t) ->
       Printf.printf "%s %d:%d-%d:%d\n" (t2s v) sr sc er ec;
       ())
    () ts

let rec print_p p lvl =
  List.fold_left
    (fun _ ->
      fun dec ->
       print_dec dec lvl;
       ())
    () p

and print_dec (dec : Ast.dec) lvl =
  print_string (String.make (2 * lvl) ' ');
  match dec.v with
  | Ast.Let (n, e) ->
      Printf.printf "let : name = %s\n" n;
      print_expr e (lvl + 1)
  | Ast.Var (n, e) ->
      Printf.printf "var : name = %s\n" n;
      print_expr e (lvl + 1)
  | Ast.VarSet (v, e) ->
      Printf.printf "var_set\n";
      print_v v (lvl + 1);
      print_expr e (lvl + 1)
  | Ast.Print e ->
      Printf.printf "print\n";
      print_expr e (lvl + 1)
  | Ast.Println e ->
      Printf.printf "println\n";
      print_expr e (lvl + 1)
  | Ast.If (Ast.IfThen (e, ds)) ->
      Printf.printf "if then\n";
      print_expr e (lvl + 1);
      print_p ds (lvl + 1)
  | Ast.If (Ast.IfThenElse (e, ds, ds2)) ->
      Printf.printf "if then else\n";
      print_expr e (lvl + 1);
      print_p ds (lvl + 1);
      print_p ds2 (lvl + 1)
  | Ast.While (e, ds) ->
      Printf.printf "while\n";
      print_expr e (lvl + 1);
      print_p ds (lvl + 1)

and print_expr expr lvl : unit =
  print_string (String.make (2 * lvl) ' ');
  match expr.v with
  | Ast.Num n -> Printf.printf "%d\n" n
  | Ast.Id id ->
      Printf.printf "id\n";
      print_v id (lvl + 1)
  | Ast.True -> Printf.printf "true\n"
  | Ast.False -> Printf.printf "false\n"
  | Ast.Neg e ->
      Printf.printf "neg\n";
      print_expr e (lvl + 1)
  | Ast.Pos e ->
      Printf.printf "pos\n";
      print_expr e (lvl + 1)
  | Ast.Not e ->
      Printf.printf "not\n";
      print_expr e (lvl + 1)
  | Ast.Eq (e1, e2) ->
      Printf.printf "eq\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ast.Gt (e1, e2) ->
      Printf.printf "gt\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ast.Lt (e1, e2) ->
      Printf.printf "lt\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ast.Add (e1, e2) ->
      Printf.printf "add\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ast.Sub (e1, e2) ->
      Printf.printf "sub\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ast.Mul (e1, e2) ->
      Printf.printf "mul\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ast.Div (e1, e2) ->
      Printf.printf "div\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ast.Mod (e1, e2) ->
      Printf.printf "mod\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ast.And (e1, e2) ->
      Printf.printf "and\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ast.Or (e1, e2) ->
      Printf.printf "or\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ast.Xor (e1, e2) ->
      Printf.printf "xor\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ast.List es ->
      Printf.printf "list\n";
      List.fold_left (fun () -> fun e -> print_expr e (lvl + 1)) () es

and print_v v lvl =
  print_string (String.make (2 * lvl) ' ');
  match v with
  | Ast.Name name -> Printf.printf "%s\n" name
  | Ast.At (v', expr) ->
      Printf.printf "at\n";
      print_v v' (lvl + 1);
      print_expr expr (lvl + 1)
