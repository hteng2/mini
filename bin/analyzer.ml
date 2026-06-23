type ctx = { loop : bool; func : (Types.tt * Types.t Closure.t list) option }

exception TypeError of Errors.error
exception CtrlError of Errors.error
exception NameError of Errors.error

type 'a cid = I of 'a | E of 'a | N

let find scopes ctx name =
  match Closure.search scopes name with
  | Some x -> I x
  | None -> (
      match ctx.func with
      | None -> N
      | Some (_, scopes2) -> (
          match Closure.search scopes2 name with None -> N | Some x -> E x))

let force_type t1 t2 e = if t1 = t2 then () else raise (TypeError e)

let rec translate_type (t : Ast.mini_type) =
  match t.v with
  | Ast.MtBase "int" -> Types.Int
  | Ast.MtBase "bool" -> Types.Bool
  | Ast.MtBase "str" -> Types.Str
  | Ast.MtBase "void" -> Types.Void
  | Ast.MtList t' -> Types.List (translate_type t')
  | Ast.MtFn (t, ts) -> Types.Fn (translate_type t, List.map translate_type ts)
  | _ -> assert false

let rec analyze_id (id : Ast.identifier) scopes ctx =
  match id.v with
  | Ast.IdName name -> (
      match find scopes ctx name with
      | I t -> (t, Closure.empty (), Ir.IdName name)
      | E t ->
          let c = Closure.empty () in
          Closure.set c name ();
          (t, c, Ir.IdName name)
      | N -> raise (NameError { v = name; span = id.span }))
  | Ast.IdAt (id', expr) -> (
      let t, c1, id'' = analyze_id id' scopes ctx in
      let t2, c2, expr' = analyze_expr expr scopes ctx in
      match (t, t2) with
      | (Types.List t, m), Types.Int ->
          let c = Closure.empty () in
          Closure.merge c c1;
          Closure.merge c c2;
          ((t, m), c, Ir.IdAt (id'', expr'))
      | (Types.Str, m), Types.Int ->
          let c = Closure.empty () in
          Closure.merge c c1;
          Closure.merge c c2;
          ((Types.Char, m), c, Ir.IdAt (id'', expr'))
      | (Types.List t, m), _ ->
          raise (TypeError { v = "index must be int"; span = id.span })
      | _ -> raise (TypeError { v = "list access of non-list"; span = id.span })
      )

and analyze_expr expr scopes ctx =
  match expr.v with
  | Ast.Num n -> (Types.Int, Closure.empty (), Ir.Num n)
  | Ast.Char c -> (Types.Char, Closure.empty (), Ir.Char c)
  | Ast.Str s -> (Types.Str, Closure.empty (), Ir.Str s)
  | Ast.Name name -> (
      match find scopes ctx name with
      | I (t, _) -> (t, Closure.empty (), Ir.Name name)
      | E (t, _) ->
          let c = Closure.empty () in
          Closure.set c name ();
          (t, c, Ir.Name name)
      | N -> raise (NameError { v = name; span = expr.span }))
  | Ast.True -> (Types.Bool, Closure.empty (), Ir.True)
  | Ast.False -> (Types.Bool, Closure.empty (), Ir.False)
  | Ast.Void -> (Types.Void, Closure.empty (), Ir.Void)
  | Ast.Neg e ->
      let t, cs, e' = analyze_expr e scopes ctx in
      force_type t Types.Int { v = "expected integer"; span = e.span };
      (Types.Int, cs, Ir.Neg e')
  | Ast.Pos e ->
      let t, cs, e' = analyze_expr e scopes ctx in
      force_type t Types.Int { v = "expected integer"; span = e.span };
      (Types.Int, cs, Ir.Pos e')
  | Ast.Not e ->
      let t, cs, e' = analyze_expr e scopes ctx in
      force_type t Types.Bool { v = "expected boolean"; span = e.span };
      (Types.Int, cs, Ir.Not e')
  | Ast.Eq (e1, e2) ->
      let t1, cs1, e1' = analyze_expr e1 scopes ctx in
      let t2, cs2, e2' = analyze_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      force_type t1 t2 { v = "compared types must be equal"; span = expr.span };
      (Types.Bool, c, Ir.Eq (e1', e2'))
  | Ast.Gt (e1, e2) ->
      let t1, cs1, e1' = analyze_expr e1 scopes ctx in
      let t2, cs2, e2' = analyze_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      force_type t1 Types.Int { v = "expected integer"; span = e1.span };
      force_type t2 Types.Int { v = "expected integer"; span = e2.span };
      (Types.Bool, c, Ir.Gt (e1', e2'))
  | Ast.Lt (e1, e2) ->
      let t1, cs1, e1' = analyze_expr e1 scopes ctx in
      let t2, cs2, e2' = analyze_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      force_type t1 Types.Int { v = "expected integer"; span = e1.span };
      force_type t2 Types.Int { v = "expected integer"; span = e2.span };
      (Types.Bool, c, Ir.Lt (e1', e2'))
  | Ast.Add (e1, e2) ->
      let t1, cs1, e1' = analyze_expr e1 scopes ctx in
      let t2, cs2, e2' = analyze_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      force_type t1 Types.Int { v = "expected integer"; span = e1.span };
      force_type t2 Types.Int { v = "expected integer"; span = e2.span };
      (Types.Int, c, Ir.Add (e1', e2'))
  | Ast.Sub (e1, e2) ->
      let t1, cs1, e1' = analyze_expr e1 scopes ctx in
      let t2, cs2, e2' = analyze_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      force_type t1 Types.Int { v = "expected integer"; span = e1.span };
      force_type t2 Types.Int { v = "expected integer"; span = e2.span };
      (Types.Int, c, Ir.Sub (e1', e2'))
  | Ast.Mul (e1, e2) ->
      let t1, cs1, e1' = analyze_expr e1 scopes ctx in
      let t2, cs2, e2' = analyze_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      force_type t1 Types.Int { v = "expected integer"; span = e1.span };
      force_type t2 Types.Int { v = "expected integer"; span = e2.span };
      (Types.Int, c, Ir.Mul (e1', e2'))
  | Ast.Div (e1, e2) ->
      let t1, cs1, e1' = analyze_expr e1 scopes ctx in
      let t2, cs2, e2' = analyze_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      force_type t1 Types.Int { v = "expected integer"; span = e1.span };
      force_type t2 Types.Int { v = "expected integer"; span = e2.span };
      (Types.Int, c, Ir.Div (e1', e2'))
  | Ast.Mod (e1, e2) ->
      let t1, cs1, e1' = analyze_expr e1 scopes ctx in
      let t2, cs2, e2' = analyze_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      force_type t1 Types.Int { v = "expected integer"; span = e1.span };
      force_type t2 Types.Int { v = "expected integer"; span = e2.span };
      (Types.Int, c, Ir.Mod (e1', e2'))
  | Ast.And (e1, e2) ->
      let t1, cs1, e1' = analyze_expr e1 scopes ctx in
      let t2, cs2, e2' = analyze_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
      force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
      (Types.Bool, c, Ir.And (e1', e2'))
  | Ast.Or (e1, e2) ->
      let t1, cs1, e1' = analyze_expr e1 scopes ctx in
      let t2, cs2, e2' = analyze_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
      force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
      (Types.Bool, c, Ir.Or (e1', e2'))
  | Ast.Xor (e1, e2) ->
      let t1, cs1, e1' = analyze_expr e1 scopes ctx in
      let t2, cs2, e2' = analyze_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
      force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
      (Types.Bool, c, Ir.Xor (e1', e2'))
  | Ast.List es ->
      let t, c, es' = infer_list es expr.span scopes ctx in
      (Types.List t, c, Ir.List es')
  | Ast.At (e1, e2) -> (
      let t1, cs1, e1' = analyze_expr e1 scopes ctx in
      match t1 with
      | Types.List t ->
          let t2, cs2, e2' = analyze_expr e2 scopes ctx in
          let c = Closure.empty () in
          Closure.merge c cs1;
          Closure.merge c cs2;
          (t, c, Ir.At (e1', e2'))
      | Types.Str ->
          let t2, cs2, e2' = analyze_expr e2 scopes ctx in
          let c = Closure.empty () in
          Closure.merge c cs1;
          Closure.merge c cs2;
          (Types.Char, c, Ir.At (e1', e2'))
      | _ ->
          raise (TypeError { v = "list access of non-list"; span = expr.span }))
  | Ast.FnVal (ps, t, body) ->
      let ctx_scopes' =
        scopes @ match ctx.func with None -> [] | Some (_, scopes2) -> scopes2
      in
      let scope' = Closure.empty () in
      let names, ts =
        List.fold_left
          (fun (names, ts) ({ v = name, t; span } : Ast.param) ->
            if name <> "_" then (
              let t = translate_type t in
              Closure.set scope' name (t, Types.Const);
              (name :: names, t :: ts))
            else (names, ts))
          ([], []) ps
      in
      let t' = translate_type t in
      let c, body' =
        infer_dec body [ scope' ]
          { loop = false; func = Some (t', ctx_scopes') }
      in
      let c' = Closure.empty () in
      Closure.iter
        (fun name _ ->
          match Closure.get scope' name with
          | None -> Closure.set c' name ()
          | Some _ -> ())
        c;
      (Types.Fn (t', ts), c', Ir.FnVal (names, c', body'))
  | Ast.FnCall (fn, args) -> (
      let t, c, fn' = analyze_expr fn scopes ctx in
      match t with
      | Types.Fn (t', ts) ->
          if List.length ts <> List.length args then
            raise
              (TypeError
                 { v = "argument count does not match"; span = expr.span })
          else
            let args' =
              List.fold_left2
                (fun acc arg t ->
                  let t2, c2, arg' = analyze_expr arg scopes ctx in
                  Closure.merge c c2;
                  force_type t t2
                    { v = "argument type does not match"; span = arg.span };
                  arg' :: acc)
                [] args ts
            in
            (t', c, Ir.FnCall (fn', args'))
      | _ -> raise (TypeError { v = "call of non-function"; span = expr.span }))

and infer_list es span scopes ctx =
  match es with
  | [] -> (Types.Untyped, Closure.empty (), [])
  | e :: es' ->
      let t1, cs1, e' = analyze_expr e scopes ctx in
      let t2, cs2, es'' = infer_list es' span scopes ctx in
      if t1 <> t2 then raise (TypeError { v = "list types do not match"; span })
      else
        let c = Closure.empty () in
        Closure.merge c cs1;
        Closure.merge c cs2;
        (t1, c, e' :: es'')

and infer_dec d scopes ctx =
  match d.v with
  | Ast.Let (name, expr) -> (
      let t, c, expr' = analyze_expr expr scopes ctx in
      match Closure.get (List.nth scopes 0) name with
      | Some (_, Types.Const) | None ->
          if name = "_" then ()
          else Closure.set (List.nth scopes 0) name (t, Types.Const);
          (c, Ir.Let (name, expr'))
      | _ ->
          raise
            (TypeError
               { v = "let statement may only shadow constants"; span = d.span })
      )
  | Ast.Var (name, expr) -> (
      let t, c, expr' = analyze_expr expr scopes ctx in
      match Closure.get (List.nth scopes 0) name with
      | Some _ ->
          raise
            (NameError
               { v = "variable name already used in this scope"; span = d.span })
      | None ->
          if name = "_" then ()
          else Closure.set (List.nth scopes 0) name (t, Types.Var);
          (c, Ir.Var (name, expr')))
  | Ast.VarSet (id, expr) ->
      let t, c1, expr' = analyze_expr expr scopes ctx in
      let (t2, m), c2, id' = analyze_id id scopes ctx in
      let c = Closure.empty () in
      Closure.merge c c1;
      Closure.merge c c2;
      if m = Types.Var && t = t2 then (c, Ir.VarSet (id', expr'))
      else raise (TypeError { v = "cannot modify non-variable"; span = d.span })
  | Ast.Print expr ->
      let t, c, expr' = analyze_expr expr scopes ctx in
      force_type t Types.Str { v = "expected string"; span = d.span };
      (c, Ir.Print expr')
  | Ast.Println expr ->
      let t, c, expr' = analyze_expr expr scopes ctx in
      force_type t Types.Str { v = "expected string"; span = d.span };
      (c, Ir.Println expr')
  | Ast.If (expr, body, body2) -> (
      let t, c, expr' = analyze_expr expr scopes ctx in
      force_type t Types.Bool { v = "expected boolean"; span = expr.span };
      let c2, body' = infer_dec body scopes ctx in
      Closure.merge c c2;
      match body2 with
      | Some body2 ->
          let c3, body2' = infer_dec body2 scopes ctx in
          Closure.merge c c3;
          (c, Ir.If (expr', body', Some body2'))
      | None -> (c, Ir.If (expr', body', None)))
  | Ast.While (expr, body) ->
      let t, c, expr' = analyze_expr expr scopes ctx in
      force_type t Types.Bool { v = "expected boolean"; span = expr.span };
      let c2, body' = infer_dec body scopes { loop = true; func = ctx.func } in
      Closure.merge c c2;
      (c, Ir.While (expr', body'))
  | Ast.Break ->
      if ctx.loop then (Closure.empty (), Ir.Break)
      else
        raise
          (CtrlError { v = "break statement must be in a loop"; span = d.span })
  | Ast.Continue ->
      if ctx.loop then (Closure.empty (), Ir.Continue)
      else
        raise
          (CtrlError
             { v = "continue statement must be in a loop"; span = d.span })
  | Ast.Return expr -> (
      match ctx.func with
      | None ->
          raise
            (CtrlError
               { v = "return statement must be in a function"; span = d.span })
      | Some (t, _) ->
          let t1, c, expr' = analyze_expr expr scopes ctx in
          force_type t t1
            {
              v = "return type does not declared result type";
              span = expr.span;
            };
          (c, Ir.Return expr'))
  | Ast.Block body ->
      let c, body' = check_program body (Closure.empty () :: scopes) ctx in
      (c, Ir.Block body')

and check_program ds scopes ctx =
  match ds () with
  | Stream.End -> (Closure.empty (), fun () -> Stream.End)
  | Stream.Head (d, ds') ->
      let c, d' = infer_dec d scopes ctx in
      let c2, ds'' = check_program ds' scopes ctx in
      Closure.merge c c2;
      (c, fun () -> Stream.Head (d', ds''))

let analyze ds =
  check_program ds [ Closure.empty () ] { loop = false; func = None }
