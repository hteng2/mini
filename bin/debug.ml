open Mini

let rec print_src src =
  match src () with
  | Stream.End -> fun () -> Stream.End
  | Stream.Head (c, src') ->
      print_char c;
      let src'' = print_src src' in
      fun () -> Stream.Head (c, src'')

let t2s t =
  match t with
  | Token.Int n -> Printf.sprintf "Int %d\t" n
  | Token.Float n -> Printf.sprintf "Float %f\t" n
  | Token.Char c -> Printf.sprintf "Char %c\t" c
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
  | Token.Semicolon -> "Semicolon\t"
  | Token.Eq -> "Eq\t"
  | Token.Neq -> "Neq\t"
  | Token.Gt -> "Gt\t"
  | Token.Ge -> "Ge\t"
  | Token.Lt -> "Lt\t"
  | Token.Le -> "Le\t"
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
  | Token.Print -> "Print\t"
  | Token.Println -> "Println\t"
  | Token.If -> "If\t"
  | Token.Else -> "Else\t"
  | Token.Fn -> "Fn\t"
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

let rec print_ast p lvl = Queue.iter (fun expr -> print_expr expr lvl) p

and print_expr (expr : Ast.expr) lvl : unit =
  print_range expr.span;
  print_string (String.make (2 * lvl) ' ');
  match expr.v with
  | Ast.Int n -> Printf.printf "%d\n" n
  | Ast.Float n -> Printf.printf "%f\n" n
  | Ast.Char c -> Printf.printf "%c\n" c
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
  | Ast.Neq (e1, e2) ->
      Printf.printf "eq\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ast.Gt (e1, e2) ->
      Printf.printf "gt\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ast.Ge (e1, e2) ->
      Printf.printf "ge\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ast.Lt (e1, e2) ->
      Printf.printf "lt\n";
      print_expr e1 (lvl + 1);
      print_expr e2 (lvl + 1)
  | Ast.Le (e1, e2) ->
      Printf.printf "le\n";
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
      print_expr body (lvl + 1)
  | Ast.FnCall (e, es) ->
      Printf.printf "fncall\n";
      print_expr e (lvl + 1);
      List.fold_left (fun () -> fun e -> print_expr e (lvl + 1)) () es
  | Ast.Let (n, e) ->
      Printf.printf "let : name = %s\n" n;
      print_expr e (lvl + 1)
  | Ast.If (e, ds, ds2) ->
      Printf.printf "if then else\n";
      print_expr e (lvl + 1);
      print_expr ds (lvl + 1);
      print_expr ds2 (lvl + 1)
  | Ast.Block ds ->
      Printf.printf "block\n";
      print_ast ds (lvl + 1)

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

let rec print_ir (ds : Ir.expr Array.t) lvl =
  Array.iter (fun expr -> print_e expr lvl) ds

and print_e (expr : Ir.expr) lvl : unit =
  let ({ v = e, _; _ } : Ir.expr) = expr in
  print_string (String.make (2 * lvl) ' ');
  match e with
  | Ir.Int n -> Printf.printf "%d\n" n
  | Ir.Float n -> Printf.printf "%f\n" n
  | Ir.Char c -> Printf.printf "%c\n" c
  | Ir.Str s -> Printf.printf "%s\n" s
  | Ir.Name n -> Printf.printf "name %s\n" n
  | Ir.Bool b -> Printf.printf "%s\n" (if b then "true" else "false")
  | Ir.Void -> Printf.printf "void\n"
  | Ir.Neg e ->
      Printf.printf "neg\n";
      print_e e (lvl + 1)
  | Ir.Add (e1, e2) ->
      Printf.printf "add\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir.Sub (e1, e2) ->
      Printf.printf "sub\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir.Mul (e1, e2) ->
      Printf.printf "mul\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir.Div (e1, e2) ->
      Printf.printf "div\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir.Mod (e1, e2) ->
      Printf.printf "mod\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir.Not e ->
      Printf.printf "not\n";
      print_e e (lvl + 1)
  | Ir.And (e1, e2) ->
      Printf.printf "and\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir.Or (e1, e2) ->
      Printf.printf "or\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir.Xor (e1, e2) ->
      Printf.printf "xor\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir.Eq (e1, e2) ->
      Printf.printf "eq\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir.Neq (e1, e2) ->
      Printf.printf "neq\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir.Gt (e1, e2) ->
      Printf.printf "gt\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir.Ge (e1, e2) ->
      Printf.printf "ge\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir.Lt (e1, e2) ->
      Printf.printf "lt\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir.Le (e1, e2) ->
      Printf.printf "le\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir.List es ->
      Printf.printf "list\n";
      List.iter (fun e -> print_e e (lvl + 1)) es
  | Ir.ListAt (e1, e2) ->
      Printf.printf "listat\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir.StrAt (e1, e2) ->
      Printf.printf "strat\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir.FnVal (ps, c, body) ->
      Printf.printf "fnval\n";
      List.iter (fun p -> print_param p (lvl + 1)) ps;
      Closure.iter (fun name _ -> print_closure name (lvl + 1)) c;
      print_e body (lvl + 1)
  | Ir.FnCall (e, es) ->
      Printf.printf "fncall\n";
      print_e e (lvl + 1);
      List.iter (fun e -> print_e e (lvl + 1)) es
  | Ir.Let (n, e) ->
      Printf.printf "let : name = %s\n" n;
      print_e e (lvl + 1)
  | Ir.If (e, e1, e2) ->
      Printf.printf "if\n";
      print_e e (lvl + 1);
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir.Block ds ->
      Printf.printf "block\n";
      Array.iter (fun e -> print_e e (lvl + 1)) ds
  | Ir.Do e ->
      Printf.printf "do\n";
      print_e e (lvl + 1)
  | Ir.Noop -> Printf.printf "noop\n"

and print_param name lvl =
  print_string (String.make (2 * lvl) ' ');
  Printf.printf "param %s\n" name

and print_closure name lvl =
  print_string (String.make (2 * lvl) ' ');
  Printf.printf "closure %s\n" name

let rec print_bc p lvl = Array.iteri (fun i d -> print_e i d lvl) p

and print_e i expr lvl : unit =
  Printf.printf "%-5d" i;
  print_string (String.make (2 * lvl) ' ');
  match expr with
  | Bytecode.Int n -> Printf.printf "%d\n" n
  | Bytecode.Float n -> Printf.printf "%f\n" n
  | Bytecode.Char c -> Printf.printf "%c\n" c
  | Bytecode.Str s -> Printf.printf "%s\n" s
  | Bytecode.Name n -> Printf.printf "name %s\n" n
  | Bytecode.Bool b -> Printf.printf "%s\n" (if b then "true" else "false")
  | Bytecode.Void -> Printf.printf "void\n"
  | Bytecode.Neg -> Printf.printf "neg\n"
  | Bytecode.Not -> Printf.printf "not\n"
  | Bytecode.Eq -> Printf.printf "eq\n"
  | Bytecode.Neq -> Printf.printf "neq\n"
  | Bytecode.Gt -> Printf.printf "gt\n"
  | Bytecode.Ge -> Printf.printf "ge\n"
  | Bytecode.Lt -> Printf.printf "lt\n"
  | Bytecode.Le -> Printf.printf "le\n"
  | Bytecode.Add -> Printf.printf "add\n"
  | Bytecode.Sub -> Printf.printf "sub\n"
  | Bytecode.Mul -> Printf.printf "mul\n"
  | Bytecode.Div -> Printf.printf "div\n"
  | Bytecode.Mod -> Printf.printf "mod\n"
  | Bytecode.And -> Printf.printf "and\n"
  | Bytecode.Or -> Printf.printf "or\n"
  | Bytecode.Xor -> Printf.printf "xor\n"
  | Bytecode.List len -> Printf.printf "list %d\n" len
  | Bytecode.ListAt -> Printf.printf "list at\n"
  | Bytecode.StrAt -> Printf.printf "str at\n"
  | Bytecode.FnVal (ps, t, len) ->
      Printf.printf "fnval %d\n" len;
      List.fold_left (fun () -> fun p -> print_param p (lvl + 1)) () ps;
      Closure.iter (fun name _ -> print_closure name (lvl + 1)) t
  | Bytecode.FnCall len -> Printf.printf "fncall %d\n" len
  | Bytecode.Let v -> Printf.printf "let %s\n" v
  | Bytecode.If -> Printf.printf "if\n"
  | Bytecode.Jmp n -> Printf.printf "jmp %d\n" n
  | Bytecode.JmpBck -> Printf.printf "jmpbck\n"
  | Bytecode.Pop -> Printf.printf "pop\n"

and print_param (param : string) lvl =
  print_string (String.make (2 * lvl) ' ');
  let name = param in
  Printf.printf "param %s\n" name

and print_closure (param : string) lvl =
  print_string (String.make (2 * lvl) ' ');
  let name = param in
  Printf.printf "closure %s\n" name

let rec print_type t lvl =
  print_string (String.make (2 * lvl) ' ');
  match t with
  | Types.Int -> print_endline "int"
  | Types.Float -> print_endline "float"
  | Types.Bool -> print_endline "bool"
  | Types.Char -> print_endline "char"
  | Types.Str -> print_endline "string"
  | Types.Void -> print_endline "void"
  | Types.List t' ->
      print_endline "list";
      print_type t' (lvl + 1)
  | Types.Fn (t, ts) ->
      print_endline "fn";
      print_type t (lvl + 1);
      List.iter (fun t -> print_type t (lvl + 1)) ts
