(* Expression flattener (ir1 -> ir2)
    flattens expressions into a sequence of operations *)

open Mini

let rec visit_ds (ds : Ir1.dec Array.t) k =
  let q = Queue.create () in
  let rec h i k =
    if i < Array.length ds then
      visit_dec ds.(i) (fun d' ->
          Queue.add d' q;
          h (i + 1) k)
    else () |> k
  in
  h 0 (fun () ->
      let ds' = Array.init (Queue.length q) (fun _ -> Queue.take q) in
      ds' |> k)

and visit_dec (d : Ir1.dec) k =
  match d.v with
  | Ir1.Let (name, expr) ->
      flatten_expr expr (fun expr' -> Ir2.Let (name, expr') |> k)
  | Ir1.Var (name, expr) ->
      flatten_expr expr (fun expr' -> Ir2.Var (name, expr') |> k)
  | Ir1.VarSet (id, expr) ->
      visit_id id (fun id' ->
          flatten_expr expr (fun expr' -> Ir2.VarSet (id', expr') |> k))
  | Ir1.If (expr, body1, body2) ->
      flatten_expr expr (fun expr' ->
          visit_dec body1 (fun body1' ->
              match body2 with
              | Some d ->
                  visit_dec d (fun d' -> Ir2.If (expr', body1', Some d') |> k)
              | None -> Ir2.If (expr', body1', None) |> k))
  | Ir1.While (expr, body) ->
      flatten_expr expr (fun expr' ->
          visit_dec body (fun body' -> Ir2.While (expr', body') |> k))
  | Ir1.Break -> Ir2.Break |> k
  | Ir1.Continue -> Ir2.Continue |> k
  | Ir1.Block ds -> visit_ds ds (fun ds' -> Ir2.Block ds' |> k)
  | Ir1.Return expr -> flatten_expr expr (fun expr' -> Ir2.Return expr' |> k)

and visit_id (id : Ir1.identifier) k =
  match id with
  | Ir1.IdName s -> Ir2.IdName s |> k
  | Ir1.IdAt (id', expr) ->
      visit_id id' (fun id'' ->
          flatten_expr expr (fun expr' -> Ir2.IdAt (id'', expr') |> k))

and flatten_expr expr k =
  let q = Queue.create () in
  let rec helper ((expr, _) : Ir1.expr) (k : unit -> 'a) =
    match expr with
    | Ir1.Int n -> Queue.add (Ir2.Int n) q |> k
    | Ir1.Float n -> Queue.add (Ir2.Float n) q |> k
    | Ir1.Char c -> Queue.add (Ir2.Char c) q |> k
    | Ir1.Str s -> Queue.add (Ir2.Str s) q |> k
    | Ir1.Name name -> Queue.add (Ir2.Name name) q |> k
    | Ir1.Bool b -> Queue.add (Ir2.Bool b) q |> k
    | Ir1.Void -> Queue.add Ir2.Void q |> k
    | Ir1.Neg e -> helper e (fun () -> Queue.add Ir2.Neg q |> k)
    | Ir1.Not e -> helper e (fun () -> Queue.add Ir2.Not q |> k)
    | Ir1.Eq (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir2.Eq q |> k))
    | Ir1.Gt (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir2.Gt q |> k))
    | Ir1.Lt (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir2.Lt q |> k))
    | Ir1.Add (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir2.Add q |> k))
    | Ir1.Sub (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir2.Sub q |> k))
    | Ir1.Mul (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir2.Mul q |> k))
    | Ir1.Div (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir2.Div q |> k))
    | Ir1.Mod (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir2.Mod q |> k))
    | Ir1.And (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir2.And q |> k))
    | Ir1.Or (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir2.Or q |> k))
    | Ir1.Xor (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir2.Xor q |> k))
    | Ir1.List es ->
        let len = List.length es in
        let rec h es k =
          match es with
          | [] -> () |> k
          | e :: es' -> helper e (fun () -> h es' k)
        in
        h es (fun () -> Queue.add (Ir2.List len) q |> k)
    | Ir1.ListAt (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir2.ListAt q |> k))
    | Ir1.StrAt (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir2.StrAt q |> k))
    | Ir1.FnVal (names, c, body) ->
        visit_dec body (fun body' ->
            Queue.add (Ir2.FnVal (names, c, body')) q |> k)
    | Ir1.FnCall (fn, args) ->
        helper fn (fun () ->
            let len = List.length args in
            let rec h args k =
              match args with
              | [] -> () |> k
              | arg :: args' -> helper arg (fun () -> h args' k)
            in
            h args (fun () -> Queue.add (Ir2.FnCall len) q |> k))
  in
  helper expr (fun () ->
      Array.init (Queue.length q) (fun _ -> Queue.take q) |> k)

let transform ds = visit_ds ds (fun ds' -> ds')
