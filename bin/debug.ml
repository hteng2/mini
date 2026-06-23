let rec print_src src =
  match src () with
  | Stream.End -> fun () -> Stream.End
  | Stream.Head (c, src') ->
      print_char c;
      let src'' = print_src src' in
      fun () -> Stream.Head (c, src'')

let t2s t =
  match t with
  | Token.Num n -> Printf.sprintf "Num %d\t" n
  | Token.Str s -> Printf.sprintf "Str %s\t" s
  | Token.Name n -> Printf.sprintf "Name %s\t" n
  | Token.True -> "True\t"
  | Token.False -> "False\t"
  | Token.Lparen -> "Lparen\t"
  | Token.Rparen -> "Rparen\t"
  | Token.Lbrack -> "Lbrack\t"
  | Token.Rbrack -> "Rbrack\t"
  | Token.Lbrace -> "Lbrace\t"
  | Token.Rbrace -> "Rbrace\t"
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
  | Token.Else -> "Else\t"
  | Token.While -> "While\t"
  | Token.Break -> "Break\t"
  | Token.Continue -> "Continue\t"
  | Token.Fn -> "Fn\t"
  | Token.Return -> "Return\t"
  | Token.Import -> "Import\t"

let rec print_tokens ts =
  match ts () with
  | Stream.End -> fun () -> Stream.End
  | Stream.Head (({ v; span = sn, (sr, sc), (er, ec) } as token : Token.t), ts')
    ->
      Printf.printf "%s %d:%d-%d:%d\n" (t2s v) sr sc er ec;
      let ts'' = print_tokens ts' in
      fun () -> Stream.Head (token, ts'')

let print_range ((sn, (a, b), (c, d)) : Loc.range) =
  let s = Printf.sprintf "%d:%d - %d:%d" a b c d in
  let s2 = String.make (20 - String.length s) ' ' in
  Printf.printf "%s%s| " s s2;
  ()

let rec print_p p lvl =
  match p () with
  | Stream.Head (d, p') ->
      print_dec d lvl;
      print_p p' lvl
  | Stream.End -> ()

and print_dec (dec : Ast.dec) lvl =
  print_range dec.span;
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
      print_id v (lvl + 1);
      print_expr e (lvl + 1)
  | Ast.Print e ->
      Printf.printf "print\n";
      print_expr e (lvl + 1)
  | Ast.Println e ->
      Printf.printf "println\n";
      print_expr e (lvl + 1)
  | Ast.If (e, ds, ds2) -> (
      Printf.printf "if then else\n";
      print_expr e (lvl + 1);
      print_dec ds (lvl + 1);
      match ds2 with Some ds2 -> print_dec ds2 (lvl + 1) | None -> ())
  | Ast.While (e, ds) ->
      Printf.printf "while\n";
      print_expr e (lvl + 1);
      print_dec ds (lvl + 1)
  | Ast.Break -> Printf.printf "break\n"
  | Ast.Continue -> Printf.printf "continue\n"
  | Ast.Return e ->
      Printf.printf "return\n";
      print_expr e (lvl + 1)
  | Ast.Block ds ->
      Printf.printf "block\n";
      print_p ds (lvl + 1)

and print_expr expr lvl : unit =
  print_range expr.span;
  print_string (String.make (2 * lvl) ' ');
  match expr.v with
  | Ast.Num n -> Printf.printf "%d\n" n
  | Ast.Str s -> Printf.printf "%s\n" s
  | Ast.Name n -> Printf.printf "name %s\n" n
  | Ast.True -> Printf.printf "true\n"
  | Ast.False -> Printf.printf "false\n"
  | Ast.Void -> Printf.printf "void\n"
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
  | Ast.At (e1, e2) ->
      Printf.printf "at\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ast.FnVal (ps, t, body) ->
      Printf.printf "fnval\n";
      List.fold_left (fun () -> fun p -> print_param p (lvl + 1)) () ps;
      print_type t (lvl + 1);
      print_dec body (lvl + 1)
  | Ast.FnCall (e, es) ->
      Printf.printf "fncall\n";
      print_expr e (lvl + 1);
      List.fold_left (fun () -> fun e -> print_expr e (lvl + 1)) () es

and print_param (param : Ast.param) lvl =
  print_range param.span;
  print_string (String.make (2 * lvl) ' ');
  let name, t = param.v in
  Printf.printf "param %s\n" name;
  print_type t (lvl + 1)

and print_type t lvl =
  print_range t.span;
  print_string (String.make (2 * lvl) ' ');
  match t.v with
  | Ast.MtBase s -> Printf.printf "%s\n" s
  | Ast.MtList t' ->
      Printf.printf "list\n";
      print_type t' (lvl + 1)
  | Ast.MtFn (t', ts) ->
      Printf.printf "fn\n";
      print_type t' (lvl + 1);
      List.iter (fun t -> print_type t (lvl + 1)) ts

and print_id id lvl =
  print_range id.span;
  print_string (String.make (2 * lvl) ' ');
  match id.v with
  | Ast.IdName name -> Printf.printf "%s\n" name
  | Ast.IdAt (id', expr) ->
      Printf.printf "at\n";
      print_id id' (lvl + 1);
      print_expr expr (lvl + 1)

let rec print_ir p lvl =
  match p () with
  | Stream.Head (d, p') ->
      print_dec d lvl;
      print_ir p' lvl
  | Stream.End -> ()

and print_dec (dec : Ir.dec) lvl =
  print_string (String.make (2 * lvl) ' ');
  match dec with
  | Ir.Let (n, e) ->
      Printf.printf "let : name = %s\n" n;
      print_expr e (lvl + 1)
  | Ir.Var (n, e) ->
      Printf.printf "var : name = %s\n" n;
      print_expr e (lvl + 1)
  | Ir.VarSet (v, e) ->
      Printf.printf "var_set\n";
      print_id v (lvl + 1);
      print_expr e (lvl + 1)
  | Ir.Print e ->
      Printf.printf "print\n";
      print_expr e (lvl + 1)
  | Ir.Println e ->
      Printf.printf "println\n";
      print_expr e (lvl + 1)
  | Ir.If (e, ds, ds2) -> (
      Printf.printf "if then else\n";
      print_expr e (lvl + 1);
      print_dec ds (lvl + 1);
      match ds2 with Some ds2 -> print_dec ds2 (lvl + 1) | None -> ())
  | Ir.While (e, ds) ->
      Printf.printf "while\n";
      print_expr e (lvl + 1);
      print_dec ds (lvl + 1)
  | Ir.Break -> Printf.printf "break\n"
  | Ir.Continue -> Printf.printf "continue\n"
  | Ir.Return e ->
      Printf.printf "return\n";
      print_expr e (lvl + 1)
  | Ir.Block ds ->
      Printf.printf "block\n";
      print_ir ds (lvl + 1)

and print_expr expr lvl : unit =
  print_string (String.make (2 * lvl) ' ');
  match expr with
  | Ir.Num n -> Printf.printf "%d\n" n
  | Ir.Str s -> Printf.printf "%s\n" s
  | Ir.Name n -> Printf.printf "name %s\n" n
  | Ir.True -> Printf.printf "true\n"
  | Ir.False -> Printf.printf "false\n"
  | Ir.Void -> Printf.printf "void\n"
  | Ir.Neg e ->
      Printf.printf "neg\n";
      print_expr e (lvl + 1)
  | Ir.Pos e ->
      Printf.printf "pos\n";
      print_expr e (lvl + 1)
  | Ir.Not e ->
      Printf.printf "not\n";
      print_expr e (lvl + 1)
  | Ir.Eq (e1, e2) ->
      Printf.printf "eq\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ir.Gt (e1, e2) ->
      Printf.printf "gt\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ir.Lt (e1, e2) ->
      Printf.printf "lt\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ir.Add (e1, e2) ->
      Printf.printf "add\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ir.Sub (e1, e2) ->
      Printf.printf "sub\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ir.Mul (e1, e2) ->
      Printf.printf "mul\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ir.Div (e1, e2) ->
      Printf.printf "div\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ir.Mod (e1, e2) ->
      Printf.printf "mod\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ir.And (e1, e2) ->
      Printf.printf "and\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ir.Or (e1, e2) ->
      Printf.printf "or\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ir.Xor (e1, e2) ->
      Printf.printf "xor\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ir.List es ->
      Printf.printf "list\n";
      List.fold_left (fun () -> fun e -> print_expr e (lvl + 1)) () es
  | Ir.At (e1, e2) ->
      Printf.printf "at\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ir.FnVal (ps, t, body) ->
      Printf.printf "fnval\n";
      List.fold_left (fun () -> fun p -> print_param p (lvl + 1)) () ps;
      print_dec body (lvl + 1)
  | Ir.FnCall (e, es) ->
      Printf.printf "fncall\n";
      print_expr e (lvl + 1);
      List.fold_left (fun () -> fun e -> print_expr e (lvl + 1)) () es

and print_param (param : string) lvl =
  print_string (String.make (2 * lvl) ' ');
  let name = param in
  Printf.printf "param %s\n" name

and print_id id lvl =
  print_string (String.make (2 * lvl) ' ');
  match id with
  | Ir.IdName name -> Printf.printf "%s\n" name
  | Ir.IdAt (id', expr) ->
      Printf.printf "at\n";
      print_id id' (lvl + 1);
      print_expr expr (lvl + 1)
