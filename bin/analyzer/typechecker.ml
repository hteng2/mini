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
let rec check_id (id : Ast.identifier) scopes ctx =
  match id.v with
  | Ast.IdName name -> (
      match find scopes ctx name with
      | I t -> (t, Closure.empty (), Ir1.IdName name)
      | E t ->
          let c = Closure.empty () in
          Closure.set c name ();
          (t, c, Ir1.IdName name)
      | N -> raise (NameError { v = name; span = id.span }))
  | Ast.IdAt (id', expr) -> (
      let t, c1, id'' = check_id id' scopes ctx in
      let c2, (expr', t2) = check_expr expr scopes ctx in
      match (t, t2) with
      | (Types.List t, m), Types.Int ->
          let c = Closure.empty () in
          Closure.merge c c1;
          Closure.merge c c2;
          ((t, m), c, Ir1.IdAt (id'', (expr', t)))
      | (Types.Str, m), Types.Int ->
          let c = Closure.empty () in
          Closure.merge c c1;
          Closure.merge c c2;
          ((Types.Char, m), c, Ir1.IdAt (id'', (expr', Types.Char)))
      | (Types.List t, m), _ ->
          raise (TypeError { v = "index must be int"; span = id.span })
      | _ -> raise (TypeError { v = "list access of non-list"; span = id.span })
      )

(* apply type induction rules and convert to ops *)
and check_expr expr scopes ctx =
  match expr.v with
  | Ast.Int n -> (Closure.empty (), (Ir1.Int n, Types.Int))
  | Ast.Float n -> (Closure.empty (), (Ir1.Float n, Types.Float))
  | Ast.Char c -> (Closure.empty (), (Ir1.Char c, Types.Char))
  | Ast.Str s -> (Closure.empty (), (Ir1.Str s, Types.Str))
  | Ast.Name name -> (
      match find scopes ctx name with
      | I (t, _) -> (Closure.empty (), (Ir1.Name name, t))
      | E (t, _) ->
          let c = Closure.empty () in
          Closure.set c name ();
          (c, (Ir1.Name name, t))
      | N -> raise (NameError { v = name; span = expr.span }))
  | Ast.True -> (Closure.empty (), (Ir1.Bool true, Types.Bool))
  | Ast.False -> (Closure.empty (), (Ir1.Bool false, Types.Bool))
  | Ast.Void -> (Closure.empty (), (Ir1.Void, Types.Void))
  | Ast.Neg e -> (
      let cs, (e', t) = check_expr e scopes ctx in
      match t with
      | Types.Int | Types.Float -> (cs, (Ir1.Neg (e', t), t))
      | _ -> raise (TypeError { v = "expected int or float"; span = e.span }))
  | Ast.Pos e -> (
      let cs, (e', t) = check_expr e scopes ctx in
      match t with
      | Types.Int | Types.Float -> (cs, (e', t))
      | _ -> raise (TypeError { v = "expected int or float"; span = e.span }))
  | Ast.Not e ->
      let cs, (e', t) = check_expr e scopes ctx in
      force_type t Types.Bool { v = "expected boolean"; span = e.span };
      (cs, (Ir1.Not (e', t), t))
  | Ast.Eq (e1, e2) -> (
      let cs1, (e1', t1) = check_expr e1 scopes ctx in
      let cs2, (e2', t2) = check_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Bool, Types.Bool ->
          (c, (Ir1.Eq ((e1', t1), (e2', t2)), Types.Bool))
      | _ ->
          raise
            (TypeError
               { v = "expected int = int or bool = bool"; span = expr.span }))
  | Ast.Gt (e1, e2) -> (
      let cs1, (e1', t1) = check_expr e1 scopes ctx in
      let cs2, (e2', t2) = check_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          (c, (Ir1.Gt ((e1', t1), (e2', t2)), Types.Bool))
      | _ ->
          raise
            (TypeError
               { v = "expected int > int or float > float"; span = expr.span }))
  | Ast.Lt (e1, e2) -> (
      let cs1, (e1', t1) = check_expr e1 scopes ctx in
      let cs2, (e2', t2) = check_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          (c, (Ir1.Lt ((e1', t1), (e2', t2)), Types.Bool))
      | _ ->
          raise
            (TypeError
               { v = "expected int > int or float > float"; span = expr.span }))
  | Ast.Add (e1, e2) -> (
      let cs1, (e1', t1) = check_expr e1 scopes ctx in
      let cs2, (e2', t2) = check_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          (c, (Ir1.Add ((e1', t1), (e2', t2)), t1))
      | _ ->
          raise
            (TypeError
               { v = "expected int + int or float + float"; span = expr.span }))
  | Ast.Sub (e1, e2) -> (
      let cs1, (e1', t1) = check_expr e1 scopes ctx in
      let cs2, (e2', t2) = check_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          (c, (Ir1.Sub ((e1', t1), (e2', t2)), t1))
      | _ ->
          raise
            (TypeError
               { v = "expected int - int or float - float"; span = expr.span }))
  | Ast.Mul (e1, e2) -> (
      let cs1, (e1', t1) = check_expr e1 scopes ctx in
      let cs2, (e2', t2) = check_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          (c, (Ir1.Mul ((e1', t1), (e2', t2)), t1))
      | _ ->
          raise
            (TypeError
               { v = "expected int * int or float * float"; span = expr.span }))
  | Ast.Div (e1, e2) -> (
      let cs1, (e1', t1) = check_expr e1 scopes ctx in
      let cs2, (e2', t2) = check_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          (c, (Ir1.Div ((e1', t1), (e2', t2)), t1))
      | _ ->
          raise
            (TypeError
               { v = "expected int / int or float / float"; span = expr.span }))
  | Ast.Mod (e1, e2) ->
      let cs1, (e1', t1) = check_expr e1 scopes ctx in
      let cs2, (e2', t2) = check_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      force_type t1 Types.Int { v = "expected integer"; span = e1.span };
      force_type t2 Types.Int { v = "expected integer"; span = e2.span };
      (c, (Ir1.Mod ((e1', t1), (e2', t2)), Types.Int))
  | Ast.And (e1, e2) ->
      let cs1, (e1', t1) = check_expr e1 scopes ctx in
      let cs2, (e2', t2) = check_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
      force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
      (c, (Ir1.And ((e1', t1), (e2', t2)), Types.Bool))
  | Ast.Or (e1, e2) ->
      let cs1, (e1', t1) = check_expr e1 scopes ctx in
      let cs2, (e2', t2) = check_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
      force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
      (c, (Ir1.Or ((e1', t1), (e2', t2)), Types.Bool))
  | Ast.Xor (e1, e2) ->
      let cs1, (e1', t1) = check_expr e1 scopes ctx in
      let cs2, (e2', t2) = check_expr e2 scopes ctx in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
      force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
      (c, (Ir1.Xor ((e1', t1), (e2', t2)), Types.Bool))
  | Ast.List es ->
      let c_acc = Closure.empty () in
      let rec infer_list es es_acc t =
        match es with
        | [] -> (t, es_acc)
        | e :: es' -> (
            let c1, (e', t1) = check_expr e scopes ctx in
            match t with
            | Some t' when t1 <> t' ->
                raise
                  (TypeError { v = "list types do not match"; span = expr.span })
            | _ ->
                Closure.merge c_acc c1;
                infer_list es' ((e', t1) :: es_acc) (Some t1))
      in
      let t, es' = infer_list es [] None in
      let t' = match t with Some t' -> t' | None -> Void in
      (c_acc, (Ir1.List (List.rev es'), Types.List t'))
  | Ast.At (e1, e2) -> (
      let cs1, (e1', t1) = check_expr e1 scopes ctx in
      match t1 with
      | Types.List t ->
          let cs2, (e2', t2) = check_expr e2 scopes ctx in
          let c = Closure.empty () in
          Closure.merge c cs1;
          Closure.merge c cs2;
          force_type t2 Types.Int { v = "expected integer"; span = e2.span };
          (c, (Ir1.ListAt ((e1', t1), (e2', t2)), t))
      | Types.Str ->
          let cs2, (e2', t2) = check_expr e2 scopes ctx in
          let c = Closure.empty () in
          Closure.merge c cs1;
          Closure.merge c cs2;
          force_type t2 Types.Int { v = "expected integer"; span = e2.span };
          (c, (Ir1.StrAt ((e1', t1), (e2', t2)), Types.Char))
      | _ ->
          raise (TypeError { v = "list access of non-list"; span = expr.span }))
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
      let c, body' = check_dec body [ scope' ] (Some (t', ctx_scopes')) in
      let c' = Closure.empty () in
      (* TODO: check if this can just be copy *)
      Closure.iter
        (fun name _ ->
          match Closure.get scope' name with
          | None -> Closure.set c' name ()
          | Some _ -> ())
        c;
      (c', (Ir1.FnVal (names, c, body'), Types.Fn (t', ts)))
  | Ast.FnCall (fn, args) -> (
      let c, (fn', t) = check_expr fn scopes ctx in
      match t with
      | Types.Fn (t', ts) ->
          let args' =
            try
              List.fold_left2
                (fun acc arg t ->
                  let c2, (arg', t2) = check_expr arg scopes ctx in
                  Closure.merge c c2;
                  force_type t t2
                    { v = "argument type does not match"; span = arg.span };
                  (arg', t2) :: acc)
                [] args ts
            with Invalid_argument _ ->
              raise
                (TypeError
                   { v = "argument count does not match"; span = expr.span })
          in
          (c, (Ir1.FnCall ((fn', t), List.rev args'), t'))
      | _ -> raise (TypeError { v = "call of non-function"; span = expr.span }))

and check_dec d scopes ctx =
  match d.v with
  | Ast.Let (name, expr) -> (
      let c, (expr', t) = check_expr expr scopes ctx in
      match Closure.get (List.nth scopes 0) name with
      | Some (_, Types.Const) | None ->
          if name = "_" then ()
          else Closure.set (List.nth scopes 0) name (t, Types.Const);
          (c, { v = Ir1.Let (name, (expr', t)); span = d.span })
      | _ ->
          raise
            (TypeError
               { v = "let statement may only shadow constants"; span = d.span })
      )
  | Ast.Var (name, expr) -> (
      let c, (expr', t) = check_expr expr scopes ctx in
      match Closure.get (List.nth scopes 0) name with
      | Some _ ->
          raise
            (NameError
               { v = "variable name already used in this scope"; span = d.span })
      | None ->
          if name = "_" then ()
          else Closure.set (List.nth scopes 0) name (t, Types.Var);
          (c, { v = Ir1.Var (name, (expr', t)); span = d.span }))
  | Ast.VarSet (id, expr) ->
      let c1, (expr', t) = check_expr expr scopes ctx in
      let (t2, m), c2, id' = check_id id scopes ctx in
      let c = Closure.empty () in
      Closure.merge c c1;
      Closure.merge c c2;
      if m = Types.Var && t = t2 then
        (c, { v = Ir1.VarSet (id', (expr', t)); span = d.span })
      else raise (TypeError { v = "cannot modify non-variable"; span = d.span })
  | Ast.If (expr, body, body2) -> (
      let c, (expr', t) = check_expr expr scopes ctx in
      force_type t Types.Bool { v = "expected boolean"; span = expr.span };
      let c2, body' = check_dec body scopes ctx in
      Closure.merge c c2;
      match body2 with
      | Some body2 ->
          let c3, body2' = check_dec body2 scopes ctx in
          Closure.merge c c3;
          (c, { v = Ir1.If ((expr', t), body', Some body2'); span = d.span })
      | None -> (c, { v = Ir1.If ((expr', t), body', None); span = d.span }))
  | Ast.While (expr, body) ->
      let c, (expr', t) = check_expr expr scopes ctx in
      force_type t Types.Bool { v = "expected boolean"; span = expr.span };
      let c2, body' = check_dec body scopes ctx in
      Closure.merge c c2;
      (c, { v = Ir1.While ((expr', t), body'); span = d.span })
  | Ast.Break -> (Closure.empty (), { v = Ir1.Break; span = d.span })
  | Ast.Continue -> (Closure.empty (), { v = Ir1.Continue; span = d.span })
  | Ast.Return expr -> (
      match ctx with
      | None ->
          raise
            (TypeError
               { v = "return statement must be in a function"; span = d.span })
      | Some (t, _) ->
          let c, (expr', t1) = check_expr expr scopes ctx in
          force_type t t1
            {
              v = "return type does not match declared result type";
              span = expr.span;
            };
          (c, { v = Ir1.Return (expr', t1); span = d.span }))
  | Ast.Block body ->
      let body', c = check_decs body (Closure.empty () :: scopes) ctx in
      (c, { v = Ir1.Block body'; span = d.span })

and check_decs ds scopes ctx =
  let acc = Queue.create () in
  let rec helper () =
    match Queue.take_opt ds with
    | None -> Closure.empty ()
    | Some d ->
        let c, d' = check_dec d scopes ctx in
        Queue.add d' acc;
        let c2 = helper () in
        Closure.merge c c2;
        c
  in
  let c = helper () in
  let acc' = Array.init (Queue.length acc) (fun _ -> Queue.take acc) in
  (acc', c)

let check ds =
  let c = Closure.empty () in
  Builtins.Fns.iter
    (fun name ({ types } : Builtins.builtinFn) -> Closure.set c name types)
    Builtins.builtins;
  let ds', c' = check_decs ds [ c ] None in
  ds'
