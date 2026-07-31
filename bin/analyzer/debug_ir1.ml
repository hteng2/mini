open Mini

let rec print_ir1 (ds : Ir1.expr Array.t) lvl =
  Array.iter (fun expr -> print_e expr lvl) ds

and print_e (expr : Ir1.expr) lvl : unit =
  let ({ v = e, _; _ } : Ir1.expr) = expr in
  print_string (String.make (2 * lvl) ' ');
  match e with
  | Ir1.Int n -> Printf.printf "%d\n" n
  | Ir1.Float n -> Printf.printf "%f\n" n
  | Ir1.Char c -> Printf.printf "%c\n" c
  | Ir1.Str s -> Printf.printf "%s\n" s
  | Ir1.Name n -> Printf.printf "name %s\n" n
  | Ir1.Bool b -> Printf.printf "%s\n" (if b then "true" else "false")
  | Ir1.Void -> Printf.printf "void\n"
  | Ir1.Neg e ->
      Printf.printf "neg\n";
      print_e e (lvl + 1)
  | Ir1.Add (e1, e2) ->
      Printf.printf "add\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir1.Sub (e1, e2) ->
      Printf.printf "sub\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir1.Mul (e1, e2) ->
      Printf.printf "mul\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir1.Div (e1, e2) ->
      Printf.printf "div\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir1.Mod (e1, e2) ->
      Printf.printf "mod\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir1.Not e ->
      Printf.printf "not\n";
      print_e e (lvl + 1)
  | Ir1.And (e1, e2) ->
      Printf.printf "and\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir1.Or (e1, e2) ->
      Printf.printf "or\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir1.Xor (e1, e2) ->
      Printf.printf "xor\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir1.Eq (e1, e2) ->
      Printf.printf "eq\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir1.Neq (e1, e2) ->
      Printf.printf "neq\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir1.Gt (e1, e2) ->
      Printf.printf "gt\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir1.Ge (e1, e2) ->
      Printf.printf "ge\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir1.Lt (e1, e2) ->
      Printf.printf "lt\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir1.Le (e1, e2) ->
      Printf.printf "le\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir1.List es ->
      Printf.printf "list\n";
      List.iter (fun e -> print_e e (lvl + 1)) es
  | Ir1.ListAt (e1, e2) ->
      Printf.printf "listat\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir1.StrAt (e1, e2) ->
      Printf.printf "strat\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir1.FnVal (ps, c, body) ->
      Printf.printf "fnval\n";
      List.iter (fun p -> print_param p (lvl + 1)) ps;
      Closure.iter (fun name _ -> print_closure name (lvl + 1)) c;
      print_e body (lvl + 1)
  | Ir1.FnCall (e, es) ->
      Printf.printf "fncall\n";
      print_e e (lvl + 1);
      List.iter (fun e -> print_e e (lvl + 1)) es
  | Ir1.Let (n, e) ->
      Printf.printf "let : name = %s\n" n;
      print_e e (lvl + 1)
  | Ir1.Var (n, e) ->
      Printf.printf "var : name = %s\n" n;
      print_e e (lvl + 1)
  | Ir1.Set (id, e) ->
      Printf.printf "set\n";
      print_id id (lvl + 1);
      print_e e (lvl + 1)
  | Ir1.If (e, e1, e2) ->
      Printf.printf "if\n";
      print_e e (lvl + 1);
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir1.While (e, body) ->
      Printf.printf "while\n";
      print_e e (lvl + 1);
      print_e body (lvl + 1)
  | Ir1.Break -> Printf.printf "break\n"
  | Ir1.Continue -> Printf.printf "continue\n"
  | Ir1.Block ds ->
      Printf.printf "block\n";
      Array.iter (fun e -> print_e e (lvl + 1)) ds

and print_param name lvl =
  print_string (String.make (2 * lvl) ' ');
  Printf.printf "param %s\n" name

and print_closure name lvl =
  print_string (String.make (2 * lvl) ' ');
  Printf.printf "closure %s\n" name

and print_id id lvl =
  print_string (String.make (2 * lvl) ' ');
  match id with
  | Ir1.IdName name -> Printf.printf "%s\n" name
  | Ir1.IdAt (id', expr) ->
      Printf.printf "at\n";
      print_id id' (lvl + 1);
      print_e expr (lvl + 1)
