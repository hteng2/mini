open Minic_lib

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
  | Token.Bind -> "let\t"
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
  | Ast.Bind (n, e) ->
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

let rec print_ir (ds : Ir2.expr Array.t) lvl =
  Array.iter (fun expr -> print_e expr lvl) ds

and print_e (expr : Ir2.expr) lvl : unit =
  let ({ v = e, _; _ } : Ir2.expr) = expr in
  print_string (String.make (2 * lvl) ' ');
  match e with
  | Ir2.Int n -> Printf.printf "%d\n" n
  | Ir2.Float n -> Printf.printf "%f\n" n
  | Ir2.Char c -> Printf.printf "%c\n" c
  | Ir2.Name n -> Printf.printf "name %d\n" n
  | Ir2.Bool b -> Printf.printf "%s\n" (if b then "true" else "false")
  | Ir2.Void -> Printf.printf "void\n"
  | Ir2.INeg e ->
      Printf.printf "ineg\n";
      print_e e (lvl + 1)
  | Ir2.IAdd (e1, e2) ->
      Printf.printf "iadd\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.ISub (e1, e2) ->
      Printf.printf "isub\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.IMul (e1, e2) ->
      Printf.printf "imul\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.IDiv (e1, e2) ->
      Printf.printf "idiv\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.IMod (e1, e2) ->
      Printf.printf "imod\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.FNeg e ->
      Printf.printf "fneg\n";
      print_e e (lvl + 1)
  | Ir2.FAdd (e1, e2) ->
      Printf.printf "fadd\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.FSub (e1, e2) ->
      Printf.printf "fsub\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.FMul (e1, e2) ->
      Printf.printf "fmul\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.FDiv (e1, e2) ->
      Printf.printf "fdiv\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.Not e ->
      Printf.printf "not\n";
      print_e e (lvl + 1)
  | Ir2.And (e1, e2) ->
      Printf.printf "and\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.Or (e1, e2) ->
      Printf.printf "or\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.Xor (e1, e2) ->
      Printf.printf "xor\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.IEq (e1, e2) ->
      Printf.printf "ieq\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.INeq (e1, e2) ->
      Printf.printf "ineq\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.IGt (e1, e2) ->
      Printf.printf "igt\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.IGe (e1, e2) ->
      Printf.printf "ige\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.ILt (e1, e2) ->
      Printf.printf "ilt\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.ILe (e1, e2) ->
      Printf.printf "ile\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.FGt (e1, e2) ->
      Printf.printf "fgt\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.FGe (e1, e2) ->
      Printf.printf "fge\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.FLt (e1, e2) ->
      Printf.printf "flt\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.FLe (e1, e2) ->
      Printf.printf "fle\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.CEq (e1, e2) ->
      Printf.printf "ceq\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.CNeq (e1, e2) ->
      Printf.printf "cneq\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.CGt (e1, e2) ->
      Printf.printf "cgt\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.CGe (e1, e2) ->
      Printf.printf "cge\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.CLt (e1, e2) ->
      Printf.printf "clt\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.CLe (e1, e2) ->
      Printf.printf "cle\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.BEq (e1, e2) ->
      Printf.printf "beq\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.List es ->
      Printf.printf "list\n";
      Array.iter (fun e -> print_e e (lvl + 1)) es
  | Ir2.At (e1, e2) ->
      Printf.printf "listat\n";
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.FnVal (ps, c, body) ->
      Printf.printf "fnval %d\n" ps;
      List.iter (fun name -> print_closure name (lvl + 1)) c;
      print_e body (lvl + 1)
  | Ir2.FnCall (e, es) ->
      Printf.printf "fncall\n";
      print_e e (lvl + 1);
      List.iter (fun e -> print_e e (lvl + 1)) es
  | Ir2.FnTailCall (e, es) ->
      Printf.printf "fntailcall\n";
      print_e e (lvl + 1);
      List.iter (fun e -> print_e e (lvl + 1)) es
  | Ir2.Bind (n, e) ->
      Printf.printf "let : name = %d\n" n;
      print_e e (lvl + 1)
  | Ir2.If (e, e1, e2) ->
      Printf.printf "if\n";
      print_e e (lvl + 1);
      print_e e1 (lvl + 1);
      print_e e2 (lvl + 1)
  | Ir2.Block ds ->
      Printf.printf "block\n";
      Array.iter (fun e -> print_e e (lvl + 1)) ds
  | Ir2.Do e ->
      Printf.printf "do\n";
      print_e e (lvl + 1)
  | Ir2.Noop -> Printf.printf "noop\n"

and print_param name lvl =
  print_string (String.make (2 * lvl) ' ');
  Printf.printf "param %d\n" name

and print_closure name lvl =
  print_string (String.make (2 * lvl) ' ');
  Printf.printf "closure %d\n" name

let rec print_bc p lvl = Array.iteri (fun i d -> print_e i d lvl) p

and print_e i expr lvl : unit =
  Printf.printf "%-5d" i;
  print_string (String.make (2 * lvl) ' ');
  match expr with
  | Ir3.Int n -> Printf.printf "%d\n" n
  | Ir3.Float n -> Printf.printf "%f\n" n
  | Ir3.Char c -> Printf.printf "%c\n" c
  | Ir3.Name n -> Printf.printf "name %d\n" n
  | Ir3.Bool b -> Printf.printf "%s\n" (if b then "true" else "false")
  | Ir3.Void -> Printf.printf "void\n"
  | Ir3.INeg -> Printf.printf "ineg\n"
  | Ir3.IAdd -> Printf.printf "iadd\n"
  | Ir3.ISub -> Printf.printf "isub\n"
  | Ir3.IMul -> Printf.printf "imul\n"
  | Ir3.IDiv -> Printf.printf "idiv\n"
  | Ir3.IMod -> Printf.printf "imod\n"
  | Ir3.FNeg -> Printf.printf "fneg\n"
  | Ir3.FAdd -> Printf.printf "fadd\n"
  | Ir3.FSub -> Printf.printf "fsub\n"
  | Ir3.FMul -> Printf.printf "fmul\n"
  | Ir3.FDiv -> Printf.printf "fdiv\n"
  | Ir3.Not -> Printf.printf "not\n"
  | Ir3.And -> Printf.printf "and\n"
  | Ir3.Or -> Printf.printf "or\n"
  | Ir3.Xor -> Printf.printf "xor\n"
  | Ir3.IEq -> Printf.printf "ieq\n"
  | Ir3.INeq -> Printf.printf "ineq\n"
  | Ir3.IGt -> Printf.printf "igt\n"
  | Ir3.IGe -> Printf.printf "ige\n"
  | Ir3.ILt -> Printf.printf "ilt\n"
  | Ir3.ILe -> Printf.printf "ile\n"
  | Ir3.FGt -> Printf.printf "fgt\n"
  | Ir3.FGe -> Printf.printf "fge\n"
  | Ir3.FLt -> Printf.printf "flt\n"
  | Ir3.FLe -> Printf.printf "fle\n"
  | Ir3.CEq -> Printf.printf "ceq\n"
  | Ir3.CNeq -> Printf.printf "cneq\n"
  | Ir3.CGt -> Printf.printf "cgt\n"
  | Ir3.CGe -> Printf.printf "cge\n"
  | Ir3.CLt -> Printf.printf "clt\n"
  | Ir3.CLe -> Printf.printf "cle\n"
  | Ir3.BEq -> Printf.printf "beq\n"
  | Ir3.List len -> Printf.printf "list %d\n" len
  | Ir3.At -> Printf.printf "at\n"
  | Ir3.FnVal (ps, c, len) ->
      Printf.printf "fnval (%d) %d\n" ps len;
      Array.iter (fun name -> print_closure name (lvl + 1)) c
  | Ir3.FnCall len -> Printf.printf "fncall %d\n" len
  | Ir3.FnTailCall len -> Printf.printf "fntailcall %d\n" len
  | Ir3.Bind v -> Printf.printf "let %d\n" v
  | Ir3.If -> Printf.printf "if\n"
  | Ir3.Jmp n -> Printf.printf "jmp %d\n" n
  | Ir3.JmpBck -> Printf.printf "jmpbck\n"
  | Ir3.Pop -> Printf.printf "pop\n"

and print_param param lvl =
  print_string (String.make (2 * lvl) ' ');
  let name = param in
  Printf.printf "param %d\n" name

and print_closure param lvl =
  print_string (String.make (2 * lvl) ' ');
  let name = param in
  Printf.printf "closure %d\n" name

let rec print_type t lvl =
  print_string (String.make (2 * lvl) ' ');
  match t with
  | Types.Int -> print_endline "int"
  | Types.Float -> print_endline "float"
  | Types.Bool -> print_endline "bool"
  | Types.Char -> print_endline "char"
  | Types.Void -> print_endline "void"
  | Types.List t' ->
      print_endline "list";
      print_type t' (lvl + 1)
  | Types.Fn (t, ts) ->
      print_endline "fn";
      print_type t (lvl + 1);
      List.iter (fun t -> print_type t (lvl + 1)) ts

(* recursively print a value, indented by lvl *)
let rec print_value (v : Values.value) lvl : unit =
  let indent = String.make (2 * lvl) ' ' in
  match v with
  | Values.Int n -> Printf.printf "%sint %d\n" indent n
  | Values.Float n -> Printf.printf "%sfloat %f\n" indent n
  | Values.Bool b -> Printf.printf "%sbool %b\n" indent b
  | Values.Char c -> Printf.printf "%schar %c\n" indent c
  | Values.Void -> Printf.printf "%svoid\n" indent
  | Values.List a ->
      Printf.printf "%slist [%d]\n" indent (Array.length a);
      Array.iter (fun v -> print_value v (lvl + 1)) a
  | Values.Fn (c, loc) ->
      Printf.printf "%sfn @ %d\n" indent loc;
      Array.iteri
        (fun i v ->
          let indent = String.make (2 * (lvl + 1)) ' ' in
          Printf.printf "%s%d =\n" indent i;
          print_value v (lvl + 1))
        c
  | Values.Builtin _ -> Printf.printf "%sbuiltin\n" indent

let print_value_top v = print_value v 0
