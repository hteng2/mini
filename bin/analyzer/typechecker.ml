(* typechecker (ast -> )
    mutability and type of symbols;
    type of exprs;
    determines closures *)

open Mini

(* Semantic analysis is ctx-sensitive *)
type ctx = (Types.t * Types.t Closure.t list) option

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
  | Ast.MtBase s ->
      raise
        (TypeError
           { v = Printf.sprintf "unrecognized type %s" s; span = t.span })
  | Ast.MtList t' -> Types.List (translate_type t')
  | Ast.MtFn (t, ts) -> Types.Fn (translate_type t, List.map translate_type ts)

(* apply type induction rules and convert to ops *)
let rec check_expr ({ v; span } : Ast.expr) scopes ctx :
    unit Closure.t * Ir.expr =
  match v with
  (* atoms *)
  | Ast.Int n -> (Closure.empty (), { v = (Ir.Int n, Types.Int); span })
  | Ast.Float n -> (Closure.empty (), { v = (Ir.Float n, Types.Float); span })
  | Ast.Char c -> (Closure.empty (), { v = (Ir.Char c, Types.Char); span })
  | Ast.Str s -> (Closure.empty (), { v = (Ir.Str s, Types.Str); span })
  | Ast.Name name -> (
      match find scopes ctx name with
      | I t -> (Closure.empty (), { v = (Ir.Name name, t); span })
      | E t ->
          let c = Closure.empty () in
          Closure.set c name ();
          (c, { v = (Ir.Name name, t); span })
      | N -> raise (NameError { v = name; span }))
  | Ast.True -> (Closure.empty (), { v = (Ir.Bool true, Types.Bool); span })
  | Ast.False -> (Closure.empty (), { v = (Ir.Bool false, Types.Bool); span })
  | Ast.Void -> (Closure.empty (), { v = (Ir.Void, Types.Void); span })
  (* unops *)
  | Ast.Neg e -> (
      let cs, ({ v = e', t } as expr' : Ir.expr) = check_expr e scopes ctx in
      match t with
      | Types.Int | Types.Float -> (cs, { v = (Ir.Neg expr', t); span })
      | _ ->
          raise
            (TypeError
               {
                 v =
                   Format.sprintf "expected int or float, got %s"
                     (Types.t_to_str t);
                 span = e.span;
               }))
  | Ast.Pos e -> (
      let cs, ({ v = e', t } as expr' : Ir.expr) = check_expr e scopes ctx in
      match t with
      | Types.Int | Types.Float -> (cs, expr')
      | _ ->
          raise
            (TypeError
               {
                 v =
                   Format.sprintf "expected int or float, got %s"
                     (Types.t_to_str t);
                 span = e.span;
               }))
  | Ast.Not e ->
      let cs, ({ v = e', t } as expr' : Ir.expr) = check_expr e scopes ctx in
      force_type t Types.Bool
        {
          v = Format.sprintf "expected boolean, got %s" (Types.t_to_str t);
          span = e.span;
        };
      (cs, { v = (Ir.Not expr', t); span })
  (* biops *)
  | Ast.Eq (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Bool, Types.Bool | Types.Char, Types.Char
        ->
          (c, { v = (Ir.Eq (expr1', expr2'), Types.Bool); span })
      | _ ->
          raise
            (TypeError
               {
                 v =
                   Format.sprintf
                     "expected int = int or bool = bool, got %s = %s"
                     (Types.t_to_str t1) (Types.t_to_str t2);
                 span;
               }))
  | Ast.Neq (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Bool, Types.Bool | Types.Char, Types.Char
        ->
          (c, { v = (Ir.Neq (expr1', expr2'), Types.Bool); span })
      | _ ->
          raise
            (TypeError
               {
                 v =
                   Format.sprintf
                     "expected int = int or bool = bool, got %s = %s"
                     (Types.t_to_str t1) (Types.t_to_str t2);
                 span;
               }))
  | Ast.Gt (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float | Types.Char, Types.Char
        ->
          (c, { v = (Ir.Gt (expr1', expr2'), Types.Bool); span })
      | _ ->
          raise
            (TypeError
               {
                 v =
                   Format.sprintf
                     "expected int, int or float, float, got %s, %s"
                     (Types.t_to_str t1) (Types.t_to_str t2);
                 span;
               }))
  | Ast.Ge (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float | Types.Char, Types.Char
        ->
          (c, { v = (Ir.Ge (expr1', expr2'), Types.Bool); span })
      | _ ->
          raise
            (TypeError
               {
                 v =
                   Format.sprintf
                     "expected int, int or float, float, got %s, %s"
                     (Types.t_to_str t1) (Types.t_to_str t2);
                 span;
               }))
  | Ast.Lt (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float | Types.Char, Types.Char
        ->
          (c, { v = (Ir.Lt (expr1', expr2'), Types.Bool); span })
      | _ ->
          raise
            (TypeError
               {
                 v =
                   Format.sprintf
                     "expected int, int or float, float, got %s, %s"
                     (Types.t_to_str t1) (Types.t_to_str t2);
                 span;
               }))
  | Ast.Le (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float | Types.Char, Types.Char
        ->
          (c, { v = (Ir.Le (expr1', expr2'), Types.Bool); span })
      | _ ->
          raise
            (TypeError
               {
                 v =
                   Format.sprintf
                     "expected int, int or float, float, got %s, %s"
                     (Types.t_to_str t1) (Types.t_to_str t2);
                 span;
               }))
  | Ast.Add (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          (c, { v = (Ir.Add (expr1', expr2'), t1); span })
      | _ ->
          raise
            (TypeError
               {
                 v =
                   Format.sprintf
                     "expected int + int or float + float, got %s + %s"
                     (Types.t_to_str t1) (Types.t_to_str t2);
                 span;
               }))
  | Ast.Sub (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          (c, { v = (Ir.Sub (expr1', expr2'), t1); span })
      | _ ->
          raise
            (TypeError
               {
                 v =
                   Format.sprintf
                     "expected int - int or float - float, got %s - %s"
                     (Types.t_to_str t1) (Types.t_to_str t2);
                 span;
               }))
  | Ast.Mul (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          (c, { v = (Ir.Mul (expr1', expr2'), t1); span })
      | _ ->
          raise
            (TypeError
               {
                 v =
                   Format.sprintf
                     "expected int * int or float * float, got %s * %s"
                     (Types.t_to_str t1) (Types.t_to_str t2);
                 span;
               }))
  | Ast.Div (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          (c, { v = (Ir.Div (expr1', expr2'), t1); span })
      | _ ->
          raise
            (TypeError
               {
                 v =
                   Format.sprintf
                     "expected int / int or float / float, got %s / %s"
                     (Types.t_to_str t1) (Types.t_to_str t2);
                 span;
               }))
  | Ast.Mod (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      match (t1, t2) with
      | Types.Int, Types.Int -> (c, { v = (Ir.Mod (expr1', expr2'), t1); span })
      | _ ->
          raise
            (TypeError
               {
                 v =
                   Format.sprintf
                     "expected int %% int or float %% float, got %s %s"
                     (Types.t_to_str t1) (Types.t_to_str t2);
                 span;
               }))
  | Ast.And (e1, e2) ->
      let cs1, ({ v = e1', t1 } as expr1' : Ir.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
      force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
      (c, { v = (Ir.And (expr1', expr2'), Types.Bool); span })
  | Ast.Or (e1, e2) ->
      let cs1, ({ v = e1', t1 } as expr1' : Ir.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
      force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
      (c, { v = (Ir.Or (expr1', expr2'), Types.Bool); span })
  | Ast.Xor (e1, e2) ->
      let cs1, ({ v = e1', t1 } as expr1' : Ir.expr) =
        check_expr e1 scopes ctx
      in
      let cs2, ({ v = e2', t2 } as expr2' : Ir.expr) =
        check_expr e2 scopes ctx
      in
      let c = Closure.empty () in
      Closure.merge c cs1;
      Closure.merge c cs2;
      force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
      force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
      (c, { v = (Ir.Xor (expr1', expr2'), Types.Bool); span })
  | Ast.List es ->
      let c_acc = Closure.empty () in
      let rec infer_list es es_acc t =
        match es with
        | [] -> (t, es_acc)
        | e :: es' -> (
            let c1, ({ v = e', t1 } as expr1' : Ir.expr) =
              check_expr e scopes ctx
            in
            match t with
            | Some t' when t1 <> t' ->
                raise
                  (TypeError
                     {
                       v =
                         Format.sprintf "list types do not match, got %s and %s"
                           (Types.t_to_str t1) (Types.t_to_str t');
                       span;
                     })
            | _ ->
                Closure.merge c_acc c1;
                infer_list es' (expr1' :: es_acc) (Some t1))
      in
      let t, es' = infer_list es [] None in
      let t' = match t with Some t' -> t' | None -> Void in
      (c_acc, { v = (Ir.List (List.rev es'), Types.List t'); span })
  | Ast.At (e1, e2) -> (
      let cs1, ({ v = e1', t1 } as expr1' : Ir.expr) =
        check_expr e1 scopes ctx
      in
      match t1 with
      | Types.List t ->
          let cs2, ({ v = e2', t2 } as expr2' : Ir.expr) =
            check_expr e2 scopes ctx
          in
          let c = Closure.empty () in
          Closure.merge c cs1;
          Closure.merge c cs2;
          force_type t2 Types.Int { v = "expected integer"; span = e2.span };
          (c, { v = (Ir.ListAt (expr1', expr2'), t); span })
      | Types.Str ->
          let cs2, ({ v = e2', t2 } as expr2' : Ir.expr) =
            check_expr e2 scopes ctx
          in
          let c = Closure.empty () in
          Closure.merge c cs1;
          Closure.merge c cs2;
          force_type t2 Types.Int { v = "expected integer"; span = e2.span };
          (c, { v = (Ir.StrAt (expr1', expr2'), Types.Char); span })
      | _ ->
          raise
            (TypeError
               {
                 v = Format.sprintf "list access of %s" (Types.t_to_str t1);
                 span;
               }))
  | Ast.FnVal (ps, t, body) ->
      let ctx_scopes' =
        scopes @ match ctx with None -> [] | Some (_, scopes2) -> scopes2
      in
      let scope' = Closure.empty () in
      let names, ts =
        List.fold_right
          (fun ({ v = name, t; span } : Ast.param) (names, ts) ->
            let t = translate_type t in
            if name <> "_" then Closure.set scope' name t;
            (name :: names, t :: ts))
          ps ([], [])
      in
      let t' = translate_type t in
      let () = Closure.set scope' "self" (Types.Fn (t', ts)) in
      let c, body' = check_expr body [ scope' ] (Some (t', ctx_scopes')) in
      let c' = Closure.empty () in
      (* TODO: check if this can just be copy *)
      Closure.iter
        (fun name _ ->
          match Closure.get scope' name with
          | None -> Closure.set c' name ()
          | Some _ -> ())
        c;
      (c', { v = (Ir.FnVal (names, c, body'), Types.Fn (t', ts)); span })
  | Ast.FnCall (fn, args) -> (
      let c, ({ v = fn', t } as expr' : Ir.expr) = check_expr fn scopes ctx in
      match t with
      | Types.Fn (t', ts) ->
          let args' =
            args
            |> List.fold_left
                 (fun acc arg ->
                   let c2, arg' = check_expr arg scopes ctx in
                   Closure.merge c c2;
                   arg' :: acc)
                 []
            |> List.rev
          in
          let _ =
            try
              List.iter2
                (fun arg t ->
                  let ({ v = _, t2 } : Ir.expr) = arg in
                  if t = t2 then () else raise (Failure ""))
                args' ts
            with _ ->
              raise
                (TypeError
                   {
                     v =
                       Format.sprintf
                         "arguments do not match, expected (%s), got (%s)"
                         (ts |> List.map Types.t_to_str |> String.concat ", ")
                         (args'
                         |> List.map (fun ({ v = _, t } : Ir.expr) -> t)
                         |> List.map Types.t_to_str |> String.concat ", ");
                     span;
                   })
          in
          (c, { v = (Ir.FnCall (expr', List.rev args'), t'); span })
      | _ ->
          raise
            (TypeError
               {
                 v =
                   Format.sprintf "call of non-function, got %s"
                     (Types.t_to_str t);
                 span;
               }))
  | Ast.Let (name, expr) ->
      let c, ({ v = e', t } as expr' : Ir.expr) = check_expr expr scopes ctx in
      if name = "_" then () else Closure.set (List.nth scopes 0) name t;
      (c, { v = (Ir.Let (name, expr'), Types.Void); span })
  | Ast.If (expr, body, body2) ->
      let c, ({ v = e', t } as expr' : Ir.expr) = check_expr expr scopes ctx in
      force_type t Types.Bool { v = "expected boolean"; span = expr.span };
      let c2, ({ v = e1', t1 } as body' : Ir.expr) =
        check_expr body scopes ctx
      in
      Closure.merge c c2;
      let c3, ({ v = e2', t2 } as body2' : Ir.expr) =
        check_expr body2 scopes ctx
      in
      let t0 = if t1 = t2 then t1 else Types.Void in
      Closure.merge c c3;
      (c, { v = (Ir.If (expr', body', body2'), t0); span })
  | Ast.Block body ->
      let body', c, t = check_exprs body (Closure.empty () :: scopes) ctx in
      (c, { v = (Ir.Block body', t); span })

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

let run es =
  let c = Closure.empty () in
  Builtins.Fns.iter
    (fun name ({ types } : Builtins.builtinFn) -> Closure.set c name types)
    Builtins.builtins;
  let es', c', t = check_exprs es [ c ] None in
  es'
