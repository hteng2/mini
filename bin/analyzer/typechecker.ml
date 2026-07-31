(* typechecker (ast -> ir1)
    mutability and type of symbols;
    type of exprs;
    determines closures *)

open Mini

(* Semantic analysis is ctx-sensitive *)
type ctx = (Types.tt * Types.t Closure.t list) option

exception TypeError of Errors.error
exception NameError of Errors.error

(* is a symbol
    in (I) the environment,
    external (E) and needs a closure, or
    does not (N) exist *)
type 'a cid = I of 'a | E of 'a | N

let find scopes ctx name =
  match Closure.search scopes name with
  | Some x -> I x
  | None -> (
      match ctx with
      | None -> N
      | Some (_, scopes2) -> (
          match Closure.search scopes2 name with None -> N | Some x -> E x))

(* quickly check for type matching *)
let force_type t1 t2 e = if t1 = t2 then () else raise (TypeError e)

(* parsed types to real types *)
let rec translate_type (t : Ast.mini_type) =
  match t.v with
  | Ast.MtBase "int" -> Types.Int
  | Ast.MtBase "float" -> Types.Float
  | Ast.MtBase "bool" -> Types.Bool
  | Ast.MtBase "str" -> Types.Str
  | Ast.MtBase "void" -> Types.Void
  | Ast.MtList t' -> Types.List (translate_type t')
  | Ast.MtFn (t, ts) -> Types.Fn (translate_type t, List.map translate_type ts)
  | _ -> raise (TypeError { v = "unrecognized type"; span = t.span })

(* check existence and infer type *)
let rec check_id (id : Ast.expr) scopes ctx =
  match id.v with
  | Ast.Name name -> (
      match find scopes ctx name with
      | I t -> (t, Closure.empty (), Ir1.IdName name)
      | E t ->
          let c = Closure.empty () in
          Closure.set c name ();
          (t, c, Ir1.IdName name)
      | N -> raise (NameError { v = name; span = id.span }))
  | Ast.At (id', expr) -> (
      let t, c1, id'' = check_id id' scopes ctx in
      let c2, ({ v = e', t2; span } as expr' : Ir1.expr) =
        check_expr expr scopes ctx
      in
      match (t, t2) with
      | (Types.List t, m), Types.Int ->
          let c = Closure.empty () in
          Closure.merge c c1;
          Closure.merge c c2;
          ((t, m), c, Ir1.IdAt (id'', expr'))
      | (Types.Str, m), Types.Int ->
          let c = Closure.empty () in
          Closure.merge c c1;
          Closure.merge c c2;
          ((Types.Char, m), c, Ir1.IdAt (id'', { v = (e', Types.Char); span }))
      | (Types.List t, m), _ ->
          raise (TypeError { v = "index must be int"; span = id.span })
      | _ -> raise (TypeError { v = "list access of non-list"; span = id.span })
      )
  | _ ->
      raise (TypeError { v = "identifier is not assignable"; span = id.span })

(* apply type induction rules and convert to ops *)
and check_expr ({ v; span } : Ast.expr) scopes ctx : unit Closure.t * Ir1.expr =
  match v with
  (* atoms *)
  | Ast.Int n -> (Closure.empty (), { v = (Ir1.Int n, Types.Int); span })
  | Ast.Float n -> (Closure.empty (), { v = (Ir1.Float n, Types.Float); span })
  | Ast.Char c -> (Closure.empty (), { v = (Ir1.Char c, Types.Char); span })
  | Ast.Str s -> (Closure.empty (), { v = (Ir1.Str s, Types.Str); span })
  | Ast.Name name -> (
      match find scopes ctx name with
      | I (t, _) -> (Closure.empty (), { v = (Ir1.Name name, t); span })
      | E (t, _) ->
          let c = Closure.empty () in
          Closure.set c name ();
          (c, { v = (Ir1.Name name, t); span })
      | N -> raise (NameError { v = name; span }))
  | Ast.True -> (Closure.empty (), { v = (Ir1.Bool true, Types.Bool); span })
  | Ast.False -> (Closure.empty (), { v = (Ir1.Bool false, Types.Bool); span })
  | Ast.Void -> (Closure.empty (), { v = (Ir1.Void, Types.Void); span })
  (* unops *)
  | Ast.Neg e -> (
      let cs, ({ v = e', t } as expr' : Ir1.expr) = check_expr e scopes ctx in
      match t with
      | Types.Int | Types.Float -> (cs, { v = (Ir1.Neg expr', t); span })
      | _ -> raise (TypeError { v = "expected int or float"; span = e.span }))
  | Ast.Pos e -> (
      let cs, ({ v = e', t } as expr' : Ir1.expr) = check_expr e scopes ctx in
      match t with
      | Types.Int | Types.Float -> (cs, expr')
      | _ -> raise (TypeError { v = "expected int or float"; span = e.span }))
  | Ast.Not e ->
      let cs, ({ v = e', t } as expr' : Ir1.expr) = check_expr e scopes ctx in
      force_type t Types.Bool { v = "expected boolean"; span = e.span };
      (cs, { v = (Ir1.Not expr', t); span })
  (* biops *)
  | Ast.Eq (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir1.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir1.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Bool, Types.Bool ->
          (c, { v = (Ir1.Eq (expr1', expr2'), Types.Bool); span })
      | _ -> raise (TypeError { v = "expected int = int or bool = bool"; span })
      )
  | Ast.Neq (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir1.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir1.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Bool, Types.Bool ->
          (c, { v = (Ir1.Neq (expr1', expr2'), Types.Bool); span })
      | _ -> raise (TypeError { v = "expected int = int or bool = bool"; span })
      )
  | Ast.Gt (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir1.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir1.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          (c, { v = (Ir1.Gt (expr1', expr2'), Types.Bool); span })
      | _ -> raise (TypeError { v = "expected int, int or float, float"; span })
      )
  | Ast.Ge (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir1.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir1.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          (c, { v = (Ir1.Ge (expr1', expr2'), Types.Bool); span })
      | _ -> raise (TypeError { v = "expected int, int or float, float"; span })
      )
  | Ast.Lt (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir1.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir1.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          (c, { v = (Ir1.Lt (expr1', expr2'), Types.Bool); span })
      | _ -> raise (TypeError { v = "expected int, int or float, float"; span })
      )
  | Ast.Le (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir1.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir1.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          (c, { v = (Ir1.Le (expr1', expr2'), Types.Bool); span })
      | _ -> raise (TypeError { v = "expected int, int or float, float"; span })
      )
  | Ast.Add (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir1.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir1.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          (c, { v = (Ir1.Add (expr1', expr2'), t1); span })
      | _ ->
          raise (TypeError { v = "expected int + int or float + float"; span }))
  | Ast.Sub (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir1.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir1.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          (c, { v = (Ir1.Sub (expr1', expr2'), t1); span })
      | _ ->
          raise (TypeError { v = "expected int - int or float - float"; span }))
  | Ast.Mul (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir1.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir1.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          (c, { v = (Ir1.Mul (expr1', expr2'), t1); span })
      | _ ->
          raise (TypeError { v = "expected int * int or float * float"; span }))
  | Ast.Div (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir1.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir1.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          (c, { v = (Ir1.Div (expr1', expr2'), t1); span })
      | _ ->
          raise (TypeError { v = "expected int / int or float / float"; span }))
  | Ast.Mod (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir1.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir1.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int -> (c, { v = (Ir1.Mod (expr1', expr2'), t1); span })
      | _ ->
          raise (TypeError { v = "expected int / int or float / float"; span }))
  | Ast.And (e1, e2) ->
      let cs1, ({ v = e1', t1 } as expr1' : Ir1.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir1.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
      force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
      (c, { v = (Ir1.And (expr1', expr2'), Types.Bool); span })
  | Ast.Or (e1, e2) ->
      let cs1, ({ v = e1', t1 } as expr1' : Ir1.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir1.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
      force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
      (c, { v = (Ir1.Or (expr1', expr2'), Types.Bool); span })
  | Ast.Xor (e1, e2) ->
      let cs1, ({ v = e1', t1 } as expr1' : Ir1.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir1.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
      force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
      (c, { v = (Ir1.Xor (expr1', expr2'), Types.Bool); span })
  | Ast.List es ->
      let c_acc = Closure.empty () in
      let rec infer_list es es_acc t =
        match es with
        | [] -> (t, es_acc)
        | e :: es' -> (
            let c1, ({ v = e', t1 } as expr1' : Ir1.expr) =
              check_expr e scopes ctx
            in
            match t with
            | Some t' when t1 <> t' ->
                raise (TypeError { v = "list types do not match"; span })
            | _ ->
                Closure.merge c_acc c1;
                infer_list es' (expr1' :: es_acc) (Some t1))
      in
      let t, es' = infer_list es [] None in
      let t' = match t with Some t' -> t' | None -> Void in
      (c_acc, { v = (Ir1.List (List.rev es'), Types.List t'); span })
  | Ast.At (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir1.expr) =
        check_expr e1 scopes ctx
      in
      match t1 with
      | Types.List t ->
          let cs2, ({ v = e2', t2 } as expr2' : Ir1.expr) =
            check_expr e2 scopes ctx
          in
          let c = Closure.empty () in
          Closure.merge c cs1;
          Closure.merge c cs2;
          force_type t2 Types.Int { v = "expected integer"; span = e2.span };
          (c, { v = (Ir1.ListAt (expr1', expr2'), t); span })
      | Types.Str ->
          let cs2, ({ v = e2', t2 } as expr2' : Ir1.expr) =
            check_expr e2 scopes ctx
          in
          let c = Closure.empty () in
          Closure.merge c cs1;
          Closure.merge c cs2;
          force_type t2 Types.Int { v = "expected integer"; span = e2.span };
          (c, { v = (Ir1.StrAt (expr1', expr2'), Types.Char); span })
      | _ -> raise (TypeError { v = "list access of non-list"; span }))
  | Ast.FnVal (ps, t, body) ->
      let ctx_scopes' =
        scopes @ match ctx with None -> [] | Some (_, scopes2) -> scopes2
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
      let c, body' = check_expr body [ scope' ] (Some (t', ctx_scopes')) in
      let c' = Closure.empty () in
      (* TODO: check if this can just be copy *)
      Closure.iter
        (fun name _ ->
          match Closure.get scope' name with
          | None -> Closure.set c' name ()
          | Some _ -> ())
        c;
      (c', { v = (Ir1.FnVal (names, c, body'), Types.Fn (t', ts)); span })
  | Ast.FnCall (fn, args) -> (
      let c, ({ v = fn', t } as expr' : Ir1.expr) = check_expr fn scopes ctx in
      match t with
      | Types.Fn (t', ts) ->
          let args' =
            try
              List.fold_left2
                (fun acc arg t ->
                  let c2, ({ v = arg', t2 } as arg'' : Ir1.expr) =
                    check_expr arg scopes ctx
                  in
                  Closure.merge c c2;
                  force_type t t2
                    { v = "argument type does not match"; span = arg.span };
                  arg'' :: acc)
                [] args ts
            with Invalid_argument _ ->
              raise (TypeError { v = "argument count does not match"; span })
          in
          (c, { v = (Ir1.FnCall (expr', List.rev args'), t'); span })
      | _ -> raise (TypeError { v = "call of non-function"; span }))
  | Ast.Let (name, expr) -> (
      let c, ({ v = e', t } as expr' : Ir1.expr) = check_expr expr scopes ctx in
      match Closure.get (List.nth scopes 0) name with
      | Some (_, Types.Const) | None ->
          if name = "_" then ()
          else Closure.set (List.nth scopes 0) name (t, Types.Const);
          (c, { v = (Ir1.Let (name, expr'), Types.Void); span })
      | _ ->
          raise
            (TypeError
               {
                 v = "let statement may only shadow constants";
                 span = expr.span;
               }))
  | Ast.Var (name, expr) -> (
      let c, ({ v = e', t } as expr' : Ir1.expr) = check_expr expr scopes ctx in
      match Closure.get (List.nth scopes 0) name with
      | Some _ ->
          raise
            (NameError
               {
                 v = "variable name already used in this scope";
                 span = expr.span;
               })
      | None ->
          if name = "_" then ()
          else Closure.set (List.nth scopes 0) name (t, Types.Var);
          (c, { v = (Ir1.Var (name, expr'), Types.Void); span }))
  | Ast.Set (id, expr) ->
      let c1, ({ v = e', t } as expr' : Ir1.expr) =
        check_expr expr scopes ctx
      in
      let (t2, m), c2, id' = check_id id scopes ctx in
      let c = Closure.empty () in
      Closure.merge c c1;
      Closure.merge c c2;
      if m = Types.Var && t = t2 then
        (c, { v = (Ir1.Set (id', expr'), Types.Void); span })
      else
        raise (TypeError { v = "cannot modify non-variable"; span = expr.span })
  | Ast.If (expr, body, body2) ->
      let c, ({ v = e', t } as expr' : Ir1.expr) = check_expr expr scopes ctx in
      force_type t Types.Bool { v = "expected boolean"; span = expr.span };
      let c2, ({ v = e1', t1 } as body' : Ir1.expr) =
        check_expr body scopes ctx
      in
      Closure.merge c c2;
      let c3, ({ v = e2', t2 } as body2' : Ir1.expr) =
        check_expr body2 scopes ctx
      in
      let t0 = if t1 = t2 then t1 else Types.Void in
      Closure.merge c c3;
      (c, { v = (Ir1.If (expr', body', body2'), t0); span })
  | Ast.While (expr, body) ->
      let c, ({ v = e', t } as expr' : Ir1.expr) = check_expr expr scopes ctx in
      force_type t Types.Bool { v = "expected boolean"; span = expr.span };
      let c2, ({ v = e1', t1 } as body' : Ir1.expr) =
        check_expr body scopes ctx
      in
      Closure.merge c c2;
      (c, { v = (Ir1.While (expr', body'), t1); span })
  | Ast.Break -> (Closure.empty (), { v = (Ir1.Break, Types.Void); span })
  | Ast.Continue -> (Closure.empty (), { v = (Ir1.Continue, Types.Void); span })
  | Ast.Block body ->
      let body', c, t = check_exprs body (Closure.empty () :: scopes) ctx in
      (c, { v = (Ir1.Block body', t); span })

and check_exprs es scopes ctx =
  let acc = Queue.create () in
  let rec helper () =
    match Queue.take_opt es with
    | None -> (Closure.empty (), None)
    | Some expr -> (
        let c, expr' = check_expr expr scopes ctx in
        let _, t = expr'.v in
        Queue.add expr' acc;
        let c2, t2 = helper () in
        Closure.merge c c2;
        match t2 with None -> (c, Some t) | Some t2 -> (c, Some t2))
  in
  let c, t = helper () in
  let acc' = Array.init (Queue.length acc) (fun _ -> Queue.take acc) in
  let t' = match t with None -> Types.Void | Some t' -> t' in
  (acc', c, t')

let check es =
  let c = Closure.empty () in
  Builtins.Fns.iter
    (fun name ({ types } : Builtins.builtinFn) -> Closure.set c name types)
    Builtins.builtins;
  let es', c', t = check_exprs es [ c ] None in
  es'
