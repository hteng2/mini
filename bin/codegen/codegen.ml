(* Expression flattener (ir -> bc)
    flattens expressions into a sequence of operations *)

open Mini

let q2a q = Array.init (Queue.length q) (fun _ -> Queue.take q)

let a2q a =
  let q = Queue.create () in
  Array.iter (fun x -> Queue.add x q) a;
  q

let rec visit_es (ds : Ir2.expr Array.t) k =
  let q = Queue.create () in
  let rec h i k =
    if i < Array.length ds then
      flatten_expr ds.(i) (fun d' ->
          Queue.transfer d' q;
          h (i + 1) k)
    else () |> k
  in
  h 0 (fun () -> q |> k)

and flatten_expr expr (k : Bytecode.e Queue.t -> 'a) =
  let q = Queue.create () in
  let rec helper ({ v = expr, _; span } : Ir2.expr) (k : unit -> 'a) =
    match expr with
    | Ir2.Int n -> Queue.add (Bytecode.Int n) q |> k
    | Ir2.Float n -> Queue.add (Bytecode.Float n) q |> k
    | Ir2.Char c -> Queue.add (Bytecode.Char c) q |> k
    | Ir2.Str s -> Queue.add (Bytecode.Str s) q |> k
    | Ir2.Name name -> Queue.add (Bytecode.Name name) q |> k
    | Ir2.Bool b -> Queue.add (Bytecode.Bool b) q |> k
    | Ir2.Void -> Queue.add Bytecode.Void q |> k
    | Ir2.Neg e -> helper e (fun () -> Queue.add Bytecode.Neg q |> k)
    | Ir2.Not e -> helper e (fun () -> Queue.add Bytecode.Not q |> k)
    | Ir2.Eq (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Bytecode.Eq q |> k))
    | Ir2.Neq (e1, e2) ->
        helper e1 (fun () ->
            helper e2 (fun () -> Queue.add Bytecode.Neq q |> k))
    | Ir2.Gt (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Bytecode.Gt q |> k))
    | Ir2.Ge (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Bytecode.Ge q |> k))
    | Ir2.Lt (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Bytecode.Lt q |> k))
    | Ir2.Le (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Bytecode.Le q |> k))
    | Ir2.Add (e1, e2) ->
        helper e1 (fun () ->
            helper e2 (fun () -> Queue.add Bytecode.Add q |> k))
    | Ir2.Sub (e1, e2) ->
        helper e1 (fun () ->
            helper e2 (fun () -> Queue.add Bytecode.Sub q |> k))
    | Ir2.Mul (e1, e2) ->
        helper e1 (fun () ->
            helper e2 (fun () -> Queue.add Bytecode.Mul q |> k))
    | Ir2.Div (e1, e2) ->
        helper e1 (fun () ->
            helper e2 (fun () -> Queue.add Bytecode.Div q |> k))
    | Ir2.Mod (e1, e2) ->
        helper e1 (fun () ->
            helper e2 (fun () -> Queue.add Bytecode.Mod q |> k))
    | Ir2.And (e1, e2) ->
        helper e1 (fun () ->
            helper e2 (fun () -> Queue.add Bytecode.And q |> k))
    | Ir2.Or (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Bytecode.Or q |> k))
    | Ir2.Xor (e1, e2) ->
        helper e1 (fun () ->
            helper e2 (fun () -> Queue.add Bytecode.Xor q |> k))
    | Ir2.List es ->
        let len = List.length es in
        let rec h es k =
          match es with
          | [] -> () |> k
          | e :: es' -> helper e (fun () -> h es' k)
        in
        h es (fun () -> Queue.add (Bytecode.List len) q |> k)
    | Ir2.ListAt (e1, e2) ->
        helper e1 (fun () ->
            helper e2 (fun () -> Queue.add Bytecode.ListAt q |> k))
    | Ir2.StrAt (e1, e2) ->
        helper e1 (fun () ->
            helper e2 (fun () -> Queue.add Bytecode.StrAt q |> k))
    | Ir2.FnVal (ps, c, body) ->
        flatten_expr body (fun body' ->
            Queue.add
              (Bytecode.FnVal (ps, Array.of_list c, Queue.length body' + 1))
              q;
            Queue.transfer body' q;
            Queue.add Bytecode.JmpBck q;
            () |> k)
    | Ir2.FnCall (fn, args) ->
        helper fn (fun () ->
            let len = List.length args in
            let rec h args k =
              match args with
              | [] -> () |> k
              | arg :: args' -> helper arg (fun () -> h args' k)
            in
            h args (fun () -> Queue.add (Bytecode.FnCall len) q |> k))
    | Ir2.FnTailCall (fn, args) ->
        helper fn (fun () ->
            let len = List.length args in
            let rec h args k =
              match args with
              | [] -> () |> k
              | arg :: args' -> helper arg (fun () -> h args' k)
            in
            h args (fun () -> Queue.add (Bytecode.FnTailCall len) q |> k))
    | Ir2.Bind (name, expr) ->
        flatten_expr expr (fun expr' ->
            Queue.transfer expr' q;
            Queue.add (Bytecode.Bind name) q |> k)
    | Ir2.If (expr, body1, body2) ->
        flatten_expr expr (fun expr' ->
            flatten_expr body1 (fun body1' ->
                flatten_expr body2 (fun body2' ->
                    Queue.transfer expr' q;
                    Queue.add Bytecode.If q;
                    Queue.add (Bytecode.Jmp (Queue.length body1' + 2)) q;
                    Queue.transfer body1' q;
                    Queue.add (Bytecode.Jmp (Queue.length body2' + 1)) q;
                    Queue.transfer body2' q;
                    () |> k)))
    | Ir2.Block es -> visit_es es (fun es' -> Queue.transfer es' q |> k)
    | Ir2.Do expr ->
        flatten_expr expr (fun expr' ->
            Queue.transfer expr' q;
            Queue.add Bytecode.Pop q |> k)
    | Ir2.Noop -> () |> k
  in
  helper expr (fun () -> q |> k)

let run ds =
  visit_es ds (fun ds' ->
      Array.init (Queue.length ds') (fun _ -> Queue.take ds'))
