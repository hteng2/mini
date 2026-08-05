(* symresolver (ast -> ir1)
    symbols to ids
    determines Hashtbls *)

open Minic_lib

exception NameError of Errors.error

let rec search_scopes scopes name =
  match scopes with
  | [] -> None
  | scope :: rest -> (
      match Hashtbl.find_opt scope name with
      | Some n -> Some n
      | None -> search_scopes rest name)

(* apply type induction rules and convert to ops *)
let rec resolve_expr ({ v; span } : Ast.expr) (scope : (string, int) Hashtbl.t)
    (closure : (string, int) Hashtbl.t) (scopes : (string, int) Hashtbl.t list)
    (id : int ref) : Ir1.expr =
  match v with
  (* atoms *)
  | Ast.Int n -> { v = Ir1.Int n; span }
  | Ast.Float n -> { v = Ir1.Float n; span }
  | Ast.Char c -> { v = Ir1.Char c; span }
  | Ast.Str s -> { v = Ir1.Str s; span }
  | Ast.Name name -> (
      match Hashtbl.find_opt scope name with
      | Some n -> { v = Ir1.Name n; span }
      | None -> (
          match search_scopes scopes name with
          | Some n ->
              Hashtbl.add closure name n;
              { v = Ir1.Name n; span }
          | None -> raise (NameError { v = name; span })))
  | Ast.True -> { v = Ir1.Bool true; span }
  | Ast.False -> { v = Ir1.Bool false; span }
  | Ast.Void -> { v = Ir1.Void; span }
  (* unops *)
  | Ast.Neg e ->
      let ({ v = e' } as expr' : Ir1.expr) =
        resolve_expr e scope closure scopes id
      in
      { v = Ir1.Neg expr'; span }
  | Ast.Pos e ->
      let ({ v = e' } as expr' : Ir1.expr) =
        resolve_expr e scope closure scopes id
      in
      { v = Ir1.Pos expr'; span }
  | Ast.Not e ->
      let ({ v = e' } as expr' : Ir1.expr) =
        resolve_expr e scope closure scopes id
      in
      { v = Ir1.Not expr'; span }
  (* biops *)
  | Ast.Eq (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) =
        resolve_expr e1 scope closure scopes id
      in
      let ({ v = e2' } as expr2' : Ir1.expr) =
        resolve_expr e2 scope closure scopes id
      in
      { v = Ir1.Eq (expr1', expr2'); span }
  | Ast.Neq (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) =
        resolve_expr e1 scope closure scopes id
      in
      let ({ v = e2' } as expr2' : Ir1.expr) =
        resolve_expr e2 scope closure scopes id
      in
      { v = Ir1.Neq (expr1', expr2'); span }
  | Ast.Gt (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) =
        resolve_expr e1 scope closure scopes id
      in
      let ({ v = e2' } as expr2' : Ir1.expr) =
        resolve_expr e2 scope closure scopes id
      in
      { v = Ir1.Gt (expr1', expr2'); span }
  | Ast.Ge (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) =
        resolve_expr e1 scope closure scopes id
      in
      let ({ v = e2' } as expr2' : Ir1.expr) =
        resolve_expr e2 scope closure scopes id
      in
      { v = Ir1.Ge (expr1', expr2'); span }
  | Ast.Lt (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) =
        resolve_expr e1 scope closure scopes id
      in
      let ({ v = e2' } as expr2' : Ir1.expr) =
        resolve_expr e2 scope closure scopes id
      in
      { v = Ir1.Lt (expr1', expr2'); span }
  | Ast.Le (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) =
        resolve_expr e1 scope closure scopes id
      in
      let ({ v = e2' } as expr2' : Ir1.expr) =
        resolve_expr e2 scope closure scopes id
      in
      { v = Ir1.Le (expr1', expr2'); span }
  | Ast.Add (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) =
        resolve_expr e1 scope closure scopes id
      in
      let ({ v = e2' } as expr2' : Ir1.expr) =
        resolve_expr e2 scope closure scopes id
      in
      { v = Ir1.Add (expr1', expr2'); span }
  | Ast.Sub (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) =
        resolve_expr e1 scope closure scopes id
      in
      let ({ v = e2' } as expr2' : Ir1.expr) =
        resolve_expr e2 scope closure scopes id
      in
      { v = Ir1.Sub (expr1', expr2'); span }
  | Ast.Mul (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) =
        resolve_expr e1 scope closure scopes id
      in
      let ({ v = e2' } as expr2' : Ir1.expr) =
        resolve_expr e2 scope closure scopes id
      in
      { v = Ir1.Mul (expr1', expr2'); span }
  | Ast.Div (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) =
        resolve_expr e1 scope closure scopes id
      in
      let ({ v = e2' } as expr2' : Ir1.expr) =
        resolve_expr e2 scope closure scopes id
      in
      { v = Ir1.Div (expr1', expr2'); span }
  | Ast.Mod (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) =
        resolve_expr e1 scope closure scopes id
      in
      let ({ v = e2' } as expr2' : Ir1.expr) =
        resolve_expr e2 scope closure scopes id
      in
      { v = Ir1.Mod (expr1', expr2'); span }
  | Ast.And (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) =
        resolve_expr e1 scope closure scopes id
      in
      let ({ v = e2' } as expr2' : Ir1.expr) =
        resolve_expr e2 scope closure scopes id
      in
      { v = Ir1.And (expr1', expr2'); span }
  | Ast.Or (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) =
        resolve_expr e1 scope closure scopes id
      in
      let ({ v = e2' } as expr2' : Ir1.expr) =
        resolve_expr e2 scope closure scopes id
      in
      { v = Ir1.Or (expr1', expr2'); span }
  | Ast.Xor (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) =
        resolve_expr e1 scope closure scopes id
      in
      let ({ v = e2' } as expr2' : Ir1.expr) =
        resolve_expr e2 scope closure scopes id
      in
      { v = Ir1.Xor (expr1', expr2'); span }
  | Ast.List es ->
      let rec infer_list es es_acc =
        match es with
        | [] -> es_acc
        | e :: es' ->
            let ({ v = e' } as expr1' : Ir1.expr) =
              resolve_expr e scope closure scopes id
            in
            infer_list es' (expr1' :: es_acc)
      in
      let es' = infer_list es [] in
      { v = Ir1.List (List.rev es'); span }
  | Ast.At (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) =
        resolve_expr e1 scope closure scopes id
      in
      let ({ v = e2' } as expr2' : Ir1.expr) =
        resolve_expr e2 scope closure scopes id
      in
      { v = Ir1.At (expr1', expr2'); span }
  | Ast.FnVal (ps, t, body) ->
      let scope' = Hashtbl.create 0 in
      let closure' = Hashtbl.create 0 in
      let ps' =
        List.fold_right
          (fun ({ v = name, t; span } : Ast.param) names ->
            let i = !id in
            Hashtbl.add scope' name i;
            id := !id + 1;
            (i, t) :: names)
          ps []
      in
      let self = !id in
      Hashtbl.add scope' "self" self;
      id := !id + 1;
      let body' = resolve_expr body scope' closure' (scope :: scopes) id in
      let closure'' =
        Hashtbl.fold
          (fun name n acc ->
            (match Hashtbl.find_opt scope name with
            | None -> Hashtbl.add closure name n
            | Some _ -> ());
            n :: acc)
          closure' []
      in
      { v = Ir1.FnVal (ps', closure'', t, self, body'); span }
  | Ast.FnCall (fn, args) ->
      let ({ v = fn' } as expr' : Ir1.expr) =
        resolve_expr fn scope closure scopes id
      in
      let args' =
        args
        |> List.fold_left
             (fun acc arg ->
               let arg' = resolve_expr arg scope closure scopes id in
               arg' :: acc)
             []
        |> List.rev
      in
      { v = Ir1.FnCall (expr', args'); span }
  | Ast.Bind (name, expr) ->
      let ({ v = e' } as expr' : Ir1.expr) =
        resolve_expr expr scope closure scopes id
      in
      let i = !id in
      id := !id + 1;
      Hashtbl.add scope name i;
      { v = Ir1.Bind (i, expr'); span }
  | Ast.If (expr, body, body2) ->
      let ({ v = e' } as expr' : Ir1.expr) =
        resolve_expr expr scope closure scopes id
      in
      let scope' = Hashtbl.create 0 in
      let closure' = Hashtbl.create 0 in
      let ({ v = e1' } as body' : Ir1.expr) =
        resolve_expr body scope' closure' (scope :: scopes) id
      in
      Hashtbl.iter
        (fun name n ->
          match Hashtbl.find_opt scope name with
          | None -> Hashtbl.add closure name n
          | Some _ -> ())
        closure';
      let scope' = Hashtbl.create 0 in
      let closure' = Hashtbl.create 0 in
      let ({ v = e2' } as body2' : Ir1.expr) =
        resolve_expr body2 scope' closure' (scope :: scopes) id
      in
      Hashtbl.iter
        (fun name n ->
          match Hashtbl.find_opt scope name with
          | None -> Hashtbl.add closure name n
          | Some _ -> ())
        closure';
      { v = Ir1.If (expr', body', body2'); span }
  | Ast.Block body ->
      let scope' = Hashtbl.create 0 in
      let closure' = Hashtbl.create 0 in
      let body' = resolve_exprs body scope' closure' (scope :: scopes) id in
      Hashtbl.iter
        (fun name n ->
          match Hashtbl.find_opt scope name with
          | None -> Hashtbl.add closure name n
          | Some _ -> ())
        closure';
      { v = Ir1.Block body'; span }

and resolve_exprs es scope closure scopes id =
  let acc = Queue.create () in
  let rec helper () =
    match Queue.take_opt es with
    | None -> ()
    | Some expr ->
        let expr' = resolve_expr expr scope closure scopes id in
        Queue.add expr' acc;
        helper ()
  in
  helper ();
  Array.init (Queue.length acc) (fun _ -> Queue.take acc)

(* apply type induction rules and convert to ops *)
let rec simplify_expr ({ v; span } as expr : Ir1.expr)
    (mapping : (int, int) Hashtbl.t) (id : int ref) : Ir1.expr =
  match v with
  (* atoms *)
  | Ir1.Int _ | Ir1.Float _ | Ir1.Char _ | Ir1.Str _ | Ir1.Bool _ | Ir1.Void ->
      expr
  | Ir1.Name name -> { expr with v = Ir1.Name (Hashtbl.find mapping name) }
  (* unops *)
  | Ir1.Neg e ->
      let ({ v = e' } as expr' : Ir1.expr) = simplify_expr e mapping id in
      { v = Ir1.Neg expr'; span }
  | Ir1.Pos e ->
      let ({ v = e' } as expr' : Ir1.expr) = simplify_expr e mapping id in
      { v = Ir1.Pos expr'; span }
  | Ir1.Not e ->
      let ({ v = e' } as expr' : Ir1.expr) = simplify_expr e mapping id in
      { v = Ir1.Not expr'; span }
  (* biops *)
  | Ir1.Eq (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 mapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 mapping id in
      { v = Ir1.Eq (expr1', expr2'); span }
  | Ir1.Neq (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 mapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 mapping id in
      { v = Ir1.Neq (expr1', expr2'); span }
  | Ir1.Gt (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 mapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 mapping id in
      { v = Ir1.Gt (expr1', expr2'); span }
  | Ir1.Ge (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 mapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 mapping id in
      { v = Ir1.Ge (expr1', expr2'); span }
  | Ir1.Lt (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 mapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 mapping id in
      { v = Ir1.Lt (expr1', expr2'); span }
  | Ir1.Le (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 mapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 mapping id in
      { v = Ir1.Le (expr1', expr2'); span }
  | Ir1.Add (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 mapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 mapping id in
      { v = Ir1.Add (expr1', expr2'); span }
  | Ir1.Sub (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 mapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 mapping id in
      { v = Ir1.Sub (expr1', expr2'); span }
  | Ir1.Mul (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 mapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 mapping id in
      { v = Ir1.Mul (expr1', expr2'); span }
  | Ir1.Div (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 mapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 mapping id in
      { v = Ir1.Div (expr1', expr2'); span }
  | Ir1.Mod (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 mapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 mapping id in
      { v = Ir1.Mod (expr1', expr2'); span }
  | Ir1.And (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 mapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 mapping id in
      { v = Ir1.And (expr1', expr2'); span }
  | Ir1.Or (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 mapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 mapping id in
      { v = Ir1.Or (expr1', expr2'); span }
  | Ir1.Xor (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 mapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 mapping id in
      { v = Ir1.Xor (expr1', expr2'); span }
  | Ir1.List es ->
      let rec infer_list es es_acc =
        match es with
        | [] -> es_acc
        | e :: es' ->
            let ({ v = e' } as expr1' : Ir1.expr) =
              simplify_expr e mapping id
            in
            infer_list es' (expr1' :: es_acc)
      in
      let es' = infer_list es [] in
      { v = Ir1.List (List.rev es'); span }
  | Ir1.At (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 mapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 mapping id in
      { v = Ir1.At (expr1', expr2'); span }
  | Ir1.FnVal (ps, closure, t, self, body) ->
      let mapping' = Hashtbl.create 0 in
      let id' = ref 0 in

      let self' = !id' in
      Hashtbl.add mapping' self !id';
      incr id';

      let ps' =
        List.map
          (fun (name, t) ->
            let name' = !id' in
            Hashtbl.add mapping' name name';
            incr id';
            (name', t))
          ps
      in

      let closure' = List.map (fun name -> Hashtbl.find mapping name) closure in
      List.iter
        (fun name ->
          Hashtbl.add mapping' name !id';
          incr id')
        closure;
      let body' = simplify_expr body mapping' id' in
      { v = Ir1.FnVal (ps', closure', t, self', body'); span }
  | Ir1.FnCall (fn, args) ->
      let ({ v = fn' } as expr' : Ir1.expr) = simplify_expr fn mapping id in
      let args' =
        args
        |> List.fold_left
             (fun acc arg ->
               let arg' = simplify_expr arg mapping id in
               arg' :: acc)
             []
        |> List.rev
      in
      { v = Ir1.FnCall (expr', args'); span }
  | Ir1.Bind (name, expr) ->
      let ({ v = e' } as expr' : Ir1.expr) = simplify_expr expr mapping id in
      let i = !id in
      Hashtbl.add mapping name i;
      incr id;
      { v = Ir1.Bind (i, expr'); span }
  | Ir1.If (expr, body, body2) ->
      let ({ v = e' } as expr' : Ir1.expr) = simplify_expr expr mapping id in
      let ({ v = e1' } as body' : Ir1.expr) = simplify_expr body mapping id in
      let ({ v = e2' } as body2' : Ir1.expr) = simplify_expr body2 mapping id in
      { v = Ir1.If (expr', body', body2'); span }
  | Ir1.Block body ->
      let body' = simplify_exprs body mapping id in
      { v = Ir1.Block body'; span }

and simplify_exprs es mapping id =
  Array.map (fun expr -> simplify_expr expr mapping id) es

let run es =
  let s = Hashtbl.create 0 in
  let c = Hashtbl.create 0 in
  let irs =
    List.iteri
      (fun i (bfn : Builtins.builtinFn) -> Hashtbl.add s bfn.name i)
      Builtins.builtins;
    resolve_exprs es s c [] (List.length Builtins.builtins |> ref)
  in
  let m = Hashtbl.create 0 in
  List.iteri
    (fun i (bfn : Builtins.builtinFn) -> Hashtbl.add m i i)
    Builtins.builtins;
  simplify_exprs irs m (List.length Builtins.builtins |> ref)
