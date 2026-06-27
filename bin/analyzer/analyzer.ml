open Mini

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

let force_type t1 t2 e =
  if t1 = t2 then ()
  else (
    Debug.print_type t1 0;
    Debug.print_type t2 0;
    raise (TypeError e))

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
  let q = Queue.create () in
  let rec helper (expr : Ast.expr) (k : Types.tt * Ir.closure -> 'a) =
    match expr.v with
    | Ast.Num n ->
        Queue.add (Ir.Num n) q;
        (Types.Int, Closure.empty ()) |> k
    | Ast.Char c ->
        Queue.add (Ir.Char c) q;
        (Types.Char, Closure.empty ()) |> k
    | Ast.Str s ->
        Queue.add (Ir.Str s) q;
        (Types.Str, Closure.empty ()) |> k
    | Ast.Name name ->
        (match find scopes ctx name with
          | I (t, _) ->
              Queue.add (Ir.Name name) q;
              (t, Closure.empty ())
          | E (t, _) ->
              Queue.add (Ir.Name name) q;
              let c = Closure.empty () in
              Closure.set c name ();
              (t, c)
          | N -> raise (NameError { v = name; span = expr.span }))
        |> k
    | Ast.True ->
        Queue.add (Ir.Bool true) q;
        (Types.Bool, Closure.empty ()) |> k
    | Ast.False ->
        Queue.add (Ir.Bool false) q;
        (Types.Bool, Closure.empty ()) |> k
    | Ast.Void ->
        Queue.add Ir.Void q;
        (Types.Void, Closure.empty ()) |> k
    | Ast.Neg e ->
        helper e (fun (t, cs) ->
            force_type t Types.Int { v = "expected integer"; span = e.span };
            Queue.add Ir.Neg q;
            (t, cs) |> k)
    | Ast.Pos e ->
        helper e (fun (t, cs) ->
            force_type t Types.Int { v = "expected integer"; span = e.span };
            (t, cs) |> k)
    | Ast.Not e ->
        helper e (fun (t, cs) ->
            force_type t Types.Bool { v = "expected boolean"; span = e.span };
            Queue.add Ir.Not q;
            (t, cs) |> k)
    | Ast.Eq (e1, e2) ->
        helper e1 (fun (t1, cs1) ->
            helper e2 (fun (t2, cs2) ->
                let c = Closure.empty () in
                Closure.merge c cs1;
                Closure.merge c cs2;
                force_type t1 t2
                  { v = "compared types must be equal"; span = expr.span };
                Queue.add Ir.Eq q;
                (Types.Bool, c) |> k))
    | Ast.Gt (e1, e2) ->
        helper e1 (fun (t1, cs1) ->
            helper e2 (fun (t2, cs2) ->
                let c = Closure.empty () in
                Closure.merge c cs1;
                Closure.merge c cs2;
                force_type t1 Types.Int
                  { v = "expected integer"; span = e1.span };
                force_type t2 Types.Int
                  { v = "expected integer"; span = e2.span };
                Queue.add Ir.Gt q;
                (Types.Bool, c) |> k))
    | Ast.Lt (e1, e2) ->
        helper e1 (fun (t1, cs1) ->
            helper e2 (fun (t2, cs2) ->
                let c = Closure.empty () in
                Closure.merge c cs1;
                Closure.merge c cs2;
                force_type t1 Types.Int
                  { v = "expected integer"; span = e1.span };
                force_type t2 Types.Int
                  { v = "expected integer"; span = e2.span };
                Queue.add Ir.Lt q;
                (Types.Bool, c) |> k))
    | Ast.Add (e1, e2) ->
        helper e1 (fun (t1, cs1) ->
            helper e2 (fun (t2, cs2) ->
                let c = Closure.empty () in
                Closure.merge c cs1;
                Closure.merge c cs2;
                force_type t1 Types.Int
                  { v = "expected integer"; span = e1.span };
                force_type t2 Types.Int
                  { v = "expected integer"; span = e2.span };
                Queue.add Ir.Add q;
                (Types.Int, c) |> k))
    | Ast.Sub (e1, e2) ->
        helper e1 (fun (t1, cs1) ->
            helper e2 (fun (t2, cs2) ->
                let c = Closure.empty () in
                Closure.merge c cs1;
                Closure.merge c cs2;
                force_type t1 Types.Int
                  { v = "expected integer"; span = e1.span };
                force_type t2 Types.Int
                  { v = "expected integer"; span = e2.span };
                Queue.add Ir.Sub q;
                (Types.Int, c) |> k))
    | Ast.Mul (e1, e2) ->
        helper e1 (fun (t1, cs1) ->
            helper e2 (fun (t2, cs2) ->
                let c = Closure.empty () in
                Closure.merge c cs1;
                Closure.merge c cs2;
                force_type t1 Types.Int
                  { v = "expected integer"; span = e1.span };
                force_type t2 Types.Int
                  { v = "expected integer"; span = e2.span };
                Queue.add Ir.Mul q;
                (Types.Int, c) |> k))
    | Ast.Div (e1, e2) ->
        helper e1 (fun (t1, cs1) ->
            helper e2 (fun (t2, cs2) ->
                let c = Closure.empty () in
                Closure.merge c cs1;
                Closure.merge c cs2;
                force_type t1 Types.Int
                  { v = "expected integer"; span = e1.span };
                force_type t2 Types.Int
                  { v = "expected integer"; span = e2.span };
                Queue.add Ir.Div q;
                (Types.Int, c) |> k))
    | Ast.Mod (e1, e2) ->
        helper e1 (fun (t1, cs1) ->
            helper e2 (fun (t2, cs2) ->
                let c = Closure.empty () in
                Closure.merge c cs1;
                Closure.merge c cs2;
                force_type t1 Types.Int
                  { v = "expected integer"; span = e1.span };
                force_type t2 Types.Int
                  { v = "expected integer"; span = e2.span };
                Queue.add Ir.Mod q;
                (Types.Int, c) |> k))
    | Ast.And (e1, e2) ->
        helper e1 (fun (t1, cs1) ->
            helper e2 (fun (t2, cs2) ->
                let c = Closure.empty () in
                Closure.merge c cs1;
                Closure.merge c cs2;
                force_type t1 Types.Bool
                  { v = "expected boolean"; span = e1.span };
                force_type t2 Types.Bool
                  { v = "expected boolean"; span = e1.span };
                Queue.add Ir.And q;
                (Types.Bool, c) |> k))
    | Ast.Or (e1, e2) ->
        helper e1 (fun (t1, cs1) ->
            helper e2 (fun (t2, cs2) ->
                let c = Closure.empty () in
                Closure.merge c cs1;
                Closure.merge c cs2;
                force_type t1 Types.Bool
                  { v = "expected boolean"; span = e1.span };
                force_type t2 Types.Bool
                  { v = "expected boolean"; span = e1.span };
                Queue.add Ir.Or q;
                (Types.Bool, c) |> k))
    | Ast.Xor (e1, e2) ->
        helper e1 (fun (t1, cs1) ->
            helper e2 (fun (t2, cs2) ->
                let c = Closure.empty () in
                Closure.merge c cs1;
                Closure.merge c cs2;
                force_type t1 Types.Bool
                  { v = "expected boolean"; span = e1.span };
                force_type t2 Types.Bool
                  { v = "expected boolean"; span = e1.span };
                Queue.add Ir.Xor q;
                (Types.Bool, c) |> k))
    | Ast.List es ->
        let len = List.length es in
        let rec infer_list es span k =
          match es with
          | [] -> (Types.Untyped, Closure.empty ()) |> k
          | e :: es' ->
              helper e (fun (t1, cs1) ->
                  infer_list es' span (fun (t2, cs2) ->
                      if t1 <> t2 && t2 != Types.Untyped then
                        raise
                          (TypeError { v = "list types do not match"; span })
                      else
                        let c = Closure.empty () in
                        Closure.merge c cs1;
                        Closure.merge c cs2;
                        (t1, c) |> k))
        in
        infer_list es expr.span (fun (t, c) ->
            Queue.add (Ir.List len) q;
            (Types.List t, c) |> k)
    | Ast.At (e1, e2) ->
        helper e1 (fun (t1, cs1) ->
            match t1 with
            | Types.List t ->
                helper e2 (fun (t2, cs2) ->
                    let c = Closure.empty () in
                    Closure.merge c cs1;
                    Closure.merge c cs2;
                    force_type t2 Types.Int
                      { v = "expected integer"; span = e2.span };
                    Queue.push Ir.ListAt q;
                    (t, c) |> k)
            | Types.Str ->
                helper e2 (fun (t2, cs2) ->
                    let c = Closure.empty () in
                    Closure.merge c cs1;
                    Closure.merge c cs2;
                    force_type t2 Types.Int
                      { v = "expected integer"; span = e2.span };
                    Queue.push Ir.StrAt q;
                    (Types.Char, c) |> k)
            | _ ->
                raise
                  (TypeError { v = "list access of non-list"; span = expr.span }))
    | Ast.FnVal (ps, t, body) ->
        let ctx_scopes' =
          scopes
          @ match ctx.func with None -> [] | Some (_, scopes2) -> scopes2
        in
        let scope' = Closure.empty () in
        let names, ts =
          List.fold_right
            (fun ({ v = name, t; span } : Ast.param) (names, ts) ->
              let t = translate_type t in
              if name <> "_" then Closure.set scope' name (t, Types.Const);
              (name :: names, t :: ts))
            ps ([], [])
        in
        let t' = translate_type t in
        let c, body' =
          infer_dec body [ scope' ]
            { loop = false; func = Some (t', ctx_scopes') }
        in
        let c' = Closure.empty () in
        (* TODO: check if this can just be copy *)
        Closure.iter
          (fun name _ ->
            match Closure.get scope' name with
            | None -> Closure.set c' name ()
            | Some _ -> ())
          c;
        Queue.add (Ir.FnVal (names, c, body')) q;
        (Types.Fn (t', ts), c') |> k
    | Ast.FnCall (fn, args) ->
        helper fn (fun (t, c) ->
            match t with
            | Types.Fn (t', ts) ->
                let len = List.length args in
                let rec h args ts k =
                  match (args, ts) with
                  | [], [] -> k ()
                  | arg :: args', t :: ts' ->
                      helper arg (fun (t2, c2) ->
                          Closure.merge c c2;
                          force_type t t2
                            {
                              v = "argument type does not match";
                              span = arg.span;
                            };
                          h args' ts' k)
                  | _ ->
                      raise
                        (TypeError
                           {
                             v = "argument count does not match";
                             span = expr.span;
                           })
                in
                h args ts (fun () ->
                    Queue.add (Ir.FnCall len) q;
                    (t', c) |> k)
            | _ ->
                raise
                  (TypeError { v = "call of non-function"; span = expr.span }))
  in
  helper expr (fun (t, c) ->
      (t, c, Array.init (Queue.length q) (fun _ -> Queue.take q)))

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
              v = "return type does not match declared result type";
              span = expr.span;
            };
          (c, Ir.Return expr'))
  | Ast.Block body ->
      let body' = Queue.create () in
      let c = analyze_decs body (Closure.empty () :: scopes) body' ctx in
      (c, Ir.Block body')

and analyze_decs ds scopes acc ctx =
  match Queue.take_opt ds with
  | None -> Closure.empty ()
  | Some d ->
      let c, d' = infer_dec d scopes ctx in
      Queue.add d' acc;
      let c2 = analyze_decs ds scopes acc ctx in
      Closure.merge c c2;
      c

let analyze ds =
  let c = Closure.empty () in
  Builtins.Fns.iter
    (fun name ({ types } : Builtins.builtinFn) -> Closure.set c name types)
    Builtins.builtins;
  let body = Queue.create () in
  let _ = analyze_decs ds [ c ] body { loop = false; func = None } in
  body
