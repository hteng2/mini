(* Expression flattener (ir1 -> ir2)
    flattens expressions into a sequence of operations *)

open Mini

let q2a q = Array.init (Queue.length q) (fun _ -> Queue.take q)

let a2q a =
  let q = Queue.create () in
  Array.iter (fun x -> Queue.add x q) a;
  q

let rec visit_es (ds : Ir1.expr Array.t) k =
  let q = Queue.create () in
  let rec h i k =
    if i < Array.length ds then
      flatten_expr ds.(i) (fun d' ->
          Queue.transfer d' q;
          h (i + 1) k)
    else () |> k
  in
  h 0 (fun () ->
      let ds' = q2a q in
      ds' |> k)

and visit_id (id : Ir1.identifier) k =
  let rec h idxs =
    match id with
    | Ir1.IdName s -> (s, idxs) |> k
    | Ir1.IdAt (id', expr) -> flatten_expr expr (fun expr' -> h (expr' :: idxs))
  in
  h []

and flatten_expr expr (k : Ir2.e Queue.t -> 'a) =
  let q = Queue.create () in
  let rec helper ({ v = expr, _; span } : Ir1.expr) (k : unit -> 'a) =
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
    | Ir1.Neq (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir2.Neq q |> k))
    | Ir1.Gt (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir2.Gt q |> k))
    | Ir1.Ge (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir2.Ge q |> k))
    | Ir1.Lt (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir2.Lt q |> k))
    | Ir1.Le (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir2.Le q |> k))
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
        flatten_expr body (fun body' ->
            Queue.add (Ir2.FnVal (names, c, Queue.length body' + 1)) q;
            Queue.transfer body' q;
            Queue.add Ir2.JmpBck q;
            () |> k)
    | Ir1.FnCall (fn, args) ->
        helper fn (fun () ->
            let len = List.length args in
            let rec h args k =
              match args with
              | [] -> () |> k
              | arg :: args' -> helper arg (fun () -> h args' k)
            in
            h args (fun () -> Queue.add (Ir2.FnCall len) q |> k))
    | Ir1.Let (name, expr) ->
        flatten_expr expr (fun expr' ->
            Queue.transfer expr' q;
            Queue.add (Ir2.Let name) q |> k)
    | Ir1.Var (name, expr) ->
        flatten_expr expr (fun expr' ->
            Queue.transfer expr' q;
            Queue.add (Ir2.Store (name, 0)) q |> k)
    | Ir1.Set (id, expr) ->
        visit_id id (fun (id', idxs) ->
            flatten_expr expr (fun expr' ->
                let len = List.length idxs in
                List.iter (fun e -> Queue.transfer e q) idxs;
                Queue.transfer expr' q;
                Queue.add (Ir2.Store (id', len)) q;
                () |> k))
    | Ir1.If (expr, body1, body2) ->
        flatten_expr expr (fun expr' ->
            flatten_expr body1 (fun body1' ->
                flatten_expr body2 (fun body2' ->
                    Queue.transfer expr' q;
                    Queue.add Ir2.If q;
                    Queue.add (Ir2.Jmp (Queue.length body1' + 2)) q;
                    Queue.transfer body1' q;
                    Queue.add (Ir2.Jmp (Queue.length body2' + 1)) q;
                    Queue.transfer body2' q;
                    () |> k)))
    | Ir1.While (expr, body) ->
        flatten_expr expr (fun expr' ->
            flatten_expr body (fun body' ->
                let len_expr = Queue.length expr' in
                let len_body = Queue.length body' in
                Queue.transfer expr' q;
                Queue.add Ir2.If q;
                Queue.add (Ir2.Jmp (len_body + 2)) q;
                (let rec h i =
                   match Queue.take_opt body' with
                   | None -> ()
                   | Some (Ir2.Label Ir2.BREAK) ->
                       Queue.add (Ir2.Jmp (len_body - i + 1)) q;
                       h (i + 1)
                   | Some (Ir2.Label Ir2.CONT) ->
                       Queue.add (Ir2.Jmp (-len_expr - i - 2)) q;
                       h (i + 1)
                   | Some x ->
                       Queue.add x q;
                       h (i + 1)
                 in
                 h 0);
                Queue.add (Ir2.Jmp (-len_expr - len_body - 2)) q;
                () |> k))
    | Ir1.Break -> Queue.add (Ir2.Label Ir2.BREAK) q |> k
    | Ir1.Continue -> Queue.add (Ir2.Label Ir2.CONT) q |> k
    | Ir1.Block es -> visit_es es (fun es' -> Queue.transfer (a2q es') q |> k)
  in
  helper expr (fun () -> q |> k)

let transform ds = visit_es ds (fun ds' -> ds')
