(* typechecker (ir1 -> ir2)
    type of exprs*)

open Mini

exception TypeError of Errors.error

let rec search_scopes scopes name =
  match scopes with
  | [] -> None
  | scope :: rest -> (
      match Hashtbl.find_opt scope name with
      | Some n -> Some n
      | None -> search_scopes rest name)

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
let rec check_expr ({ v; span } : Ir1.expr)
    (scopes : (int, Types.t) Hashtbl.t list) : Ir2.expr =
  match v with
  (* atoms *)
  | Ir1.Int n -> { v = (Ir2.Int n, Types.Int); span }
  | Ir1.Float n -> { v = (Ir2.Float n, Types.Float); span }
  | Ir1.Char c -> { v = (Ir2.Char c, Types.Char); span }
  | Ir1.Str s -> { v = (Ir2.Str s, Types.Str); span }
  | Ir1.Name name -> (
      match search_scopes scopes name with
      | Some t -> { v = (Ir2.Name name, t); span }
      | None -> assert false)
  | Ir1.Bool b -> { v = (Ir2.Bool b, Types.Bool); span }
  | Ir1.Void -> { v = (Ir2.Void, Types.Void); span }
  (* unops *)
  | Ir1.Neg e -> (
      let ({ v = e', t } as expr' : Ir2.expr) = check_expr e scopes in
      match t with
      | Types.Int | Types.Float -> { v = (Ir2.Neg expr', t); span }
      | _ ->
          raise
            (TypeError
               {
                 v =
                   Format.sprintf "expected int or float, got %s"
                     (Types.t_to_str t);
                 span = e.span;
               }))
  | Ir1.Not e ->
      let ({ v = e', t } as expr' : Ir2.expr) = check_expr e scopes in
      force_type t Types.Bool
        {
          v = Format.sprintf "expected boolean, got %s" (Types.t_to_str t);
          span = e.span;
        };
      { v = (Ir2.Not expr', t); span }
  (* biops *)
  | Ir1.Eq (e1, e2) -> (
      let ({ v = e1', t1 } as expr1' : Ir2.expr) = check_expr e1 scopes in
      let ({ v = e2', t2 } as expr2' : Ir2.expr) = check_expr e2 scopes in
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Bool, Types.Bool | Types.Char, Types.Char
        ->
          { v = (Ir2.Eq (expr1', expr2'), Types.Bool); span }
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
  | Ir1.Neq (e1, e2) -> (
      let ({ v = e1', t1 } as expr1' : Ir2.expr) = check_expr e1 scopes in
      let ({ v = e2', t2 } as expr2' : Ir2.expr) = check_expr e2 scopes in
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Bool, Types.Bool | Types.Char, Types.Char
        ->
          { v = (Ir2.Neq (expr1', expr2'), Types.Bool); span }
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
  | Ir1.Gt (e1, e2) -> (
      let ({ v = e1', t1 } as expr1' : Ir2.expr) = check_expr e1 scopes in
      let ({ v = e2', t2 } as expr2' : Ir2.expr) = check_expr e2 scopes in
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float | Types.Char, Types.Char
        ->
          { v = (Ir2.Gt (expr1', expr2'), Types.Bool); span }
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
  | Ir1.Ge (e1, e2) -> (
      let ({ v = e1', t1 } as expr1' : Ir2.expr) = check_expr e1 scopes in
      let ({ v = e2', t2 } as expr2' : Ir2.expr) = check_expr e2 scopes in
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float | Types.Char, Types.Char
        ->
          { v = (Ir2.Ge (expr1', expr2'), Types.Bool); span }
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
  | Ir1.Lt (e1, e2) -> (
      let ({ v = e1', t1 } as expr1' : Ir2.expr) = check_expr e1 scopes in
      let ({ v = e2', t2 } as expr2' : Ir2.expr) = check_expr e2 scopes in
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float | Types.Char, Types.Char
        ->
          { v = (Ir2.Lt (expr1', expr2'), Types.Bool); span }
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
  | Ir1.Le (e1, e2) -> (
      let ({ v = e1', t1 } as expr1' : Ir2.expr) = check_expr e1 scopes in
      let ({ v = e2', t2 } as expr2' : Ir2.expr) = check_expr e2 scopes in
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float | Types.Char, Types.Char
        ->
          { v = (Ir2.Le (expr1', expr2'), Types.Bool); span }
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
  | Ir1.Add (e1, e2) -> (
      let ({ v = e1', t1 } as expr1' : Ir2.expr) = check_expr e1 scopes in
      let ({ v = e2', t2 } as expr2' : Ir2.expr) = check_expr e2 scopes in
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          { v = (Ir2.Add (expr1', expr2'), t1); span }
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
  | Ir1.Sub (e1, e2) -> (
      let ({ v = e1', t1 } as expr1' : Ir2.expr) = check_expr e1 scopes in
      let ({ v = e2', t2 } as expr2' : Ir2.expr) = check_expr e2 scopes in
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          { v = (Ir2.Sub (expr1', expr2'), t1); span }
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
  | Ir1.Mul (e1, e2) -> (
      let ({ v = e1', t1 } as expr1' : Ir2.expr) = check_expr e1 scopes in
      let ({ v = e2', t2 } as expr2' : Ir2.expr) = check_expr e2 scopes in
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          { v = (Ir2.Mul (expr1', expr2'), t1); span }
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
  | Ir1.Div (e1, e2) -> (
      let ({ v = e1', t1 } as expr1' : Ir2.expr) = check_expr e1 scopes in
      let ({ v = e2', t2 } as expr2' : Ir2.expr) = check_expr e2 scopes in
      match (t1, t2) with
      | Types.Int, Types.Int | Types.Float, Types.Float ->
          { v = (Ir2.Div (expr1', expr2'), t1); span }
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
  | Ir1.Mod (e1, e2) -> (
      let ({ v = e1', t1 } as expr1' : Ir2.expr) = check_expr e1 scopes in
      let ({ v = e2', t2 } as expr2' : Ir2.expr) = check_expr e2 scopes in
      match (t1, t2) with
      | Types.Int, Types.Int -> { v = (Ir2.Mod (expr1', expr2'), t1); span }
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
  | Ir1.And (e1, e2) ->
      let ({ v = e1', t1 } as expr1' : Ir2.expr) = check_expr e1 scopes in
      let ({ v = e2', t2 } as expr2' : Ir2.expr) = check_expr e2 scopes in
      force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
      force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
      { v = (Ir2.And (expr1', expr2'), Types.Bool); span }
  | Ir1.Or (e1, e2) ->
      let ({ v = e1', t1 } as expr1' : Ir2.expr) = check_expr e1 scopes in
      let ({ v = e2', t2 } as expr2' : Ir2.expr) = check_expr e2 scopes in
      force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
      force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
      { v = (Ir2.Or (expr1', expr2'), Types.Bool); span }
  | Ir1.Xor (e1, e2) ->
      let ({ v = e1', t1 } as expr1' : Ir2.expr) = check_expr e1 scopes in
      let ({ v = e2', t2 } as expr2' : Ir2.expr) = check_expr e2 scopes in
      force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
      force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
      { v = (Ir2.Xor (expr1', expr2'), Types.Bool); span }
  | Ir1.List es ->
      let rec infer_list es es_acc t =
        match es with
        | [] -> (t, es_acc)
        | e :: es' -> (
            let ({ v = e', t1 } as expr1' : Ir2.expr) = check_expr e scopes in
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
            | _ -> infer_list es' (expr1' :: es_acc) (Some t1))
      in
      let t, es' = infer_list es [] None in
      let t' = match t with Some t' -> t' | None -> Void in
      { v = (Ir2.List (List.rev es'), Types.List t'); span }
  | Ir1.At (e1, e2) -> (
      let ({ v = e1', t1 } as expr1' : Ir2.expr) = check_expr e1 scopes in
      match t1 with
      | Types.List t ->
          let ({ v = e2', t2 } as expr2' : Ir2.expr) = check_expr e2 scopes in
          force_type t2 Types.Int { v = "expected integer"; span = e2.span };
          { v = (Ir2.ListAt (expr1', expr2'), t); span }
      | Types.Str ->
          let ({ v = e2', t2 } as expr2' : Ir2.expr) = check_expr e2 scopes in
          force_type t2 Types.Int { v = "expected integer"; span = e2.span };
          { v = (Ir2.StrAt (expr1', expr2'), Types.Char); span }
      | _ ->
          raise
            (TypeError
               {
                 v = Format.sprintf "list access of %s" (Types.t_to_str t1);
                 span;
               }))
  | Ir1.FnVal (ps, closure, t, self, body) ->
      let scope' = Hashtbl.create 0 in
      let names, ts =
        List.fold_right
          (fun (name, t) (names, ts) ->
            let t = translate_type t in
            Hashtbl.add scope' name t;
            (name :: names, t :: ts))
          ps ([], [])
      in
      let t' = translate_type t in
      let () = Hashtbl.add scope' self (Types.Fn (t', ts)) in
      let body' = check_expr body (scope' :: scopes) in
      let () =
        force_type (snd body'.v) t'
          {
            v = Printf.sprintf "fn body must return %s" (Types.t_to_str t');
            span = body'.span;
          }
      in
      let closure' = Hashtbl.fold (fun k v acc -> k :: acc) closure [] in
      {
        v = (Ir2.FnVal (names, closure', self, body'), Types.Fn (t', ts));
        span;
      }
  | Ir1.FnCall (fn, args) -> (
      let ({ v = fn', t } as expr' : Ir2.expr) = check_expr fn scopes in
      match t with
      | Types.Fn (t', ts) ->
          let args' =
            args
            |> List.fold_left
                 (fun acc arg ->
                   let arg' = check_expr arg scopes in
                   arg' :: acc)
                 []
            |> List.rev
          in
          let _ =
            try
              List.iter2
                (fun arg t ->
                  let ({ v = _, t2 } : Ir2.expr) = arg in
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
                         |> List.map (fun ({ v = _, t } : Ir2.expr) -> t)
                         |> List.map Types.t_to_str |> String.concat ", ");
                     span;
                   })
          in
          { v = (Ir2.FnCall (expr', args'), t'); span }
      | _ ->
          raise
            (TypeError
               {
                 v =
                   Format.sprintf "call of non-function, got %s"
                     (Types.t_to_str t);
                 span;
               }))
  | Ir1.Bind (name, expr) ->
      let ({ v = e', t } as expr' : Ir2.expr) = check_expr expr scopes in
      Hashtbl.add (List.nth scopes 0) name t;
      { v = (Ir2.Bind (name, expr'), Types.Void); span }
  | Ir1.If (expr, body, body2) ->
      let ({ v = e', t } as expr' : Ir2.expr) = check_expr expr scopes in
      force_type t Types.Bool { v = "expected boolean"; span = expr.span };
      let scope' = Hashtbl.create 0 in
      let ({ v = e1', t1 } as body' : Ir2.expr) =
        check_expr body (scope' :: scopes)
      in
      let scope' = Hashtbl.create 0 in
      let ({ v = e2', t2 } as body2' : Ir2.expr) =
        check_expr body2 (scope' :: scopes)
      in
      let t0 = if t1 = t2 then t1 else Types.Void in
      { v = (Ir2.If (expr', body', body2'), t0); span }
  | Ir1.Block body ->
      let scope' = Hashtbl.create 0 in
      let body', t = check_exprs body (scope' :: scopes) in
      { v = (Ir2.Block body', t); span }

and check_exprs es scopes =
  let acc = Queue.create () in
  let t =
    Array.fold_left
      (fun t e ->
        let expr' = check_expr e scopes in
        let _, t2 = expr'.v in
        Queue.add expr' acc;
        Some t2)
      None es
  in
  let acc' = Array.init (Queue.length acc) (fun _ -> Queue.take acc) in
  (acc', Option.get t)

let run es =
  let s = Hashtbl.create 0 in
  List.iteri
    (fun i (bfn : Builtins.builtinFn) -> Hashtbl.add s i bfn.fnType)
    Builtins.builtins;
  let es', t = check_exprs es [ s ] in
  es'
