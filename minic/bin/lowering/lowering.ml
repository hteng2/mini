(* Expression flattener (ir -> bc)
    flattens expressions into a sequence of operations *)

open Minic_lib

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

and flatten_expr expr (k : Ir3.ir3 Queue.t -> 'a) =
  let q = Queue.create () in
  let rec helper (expr : Ir2.expr) (k : unit -> 'a) =
    match expr.v with
    | Ir2.Int n -> Queue.add (Ir3.Int n) q |> k
    | Ir2.Float n -> Queue.add (Ir3.Float n) q |> k
    | Ir2.Char c -> Queue.add (Ir3.Char c) q |> k
    | Ir2.Name name -> Queue.add (Ir3.Name name) q |> k
    | Ir2.Bool b -> Queue.add (Ir3.Bool b) q |> k
    | Ir2.Void -> Queue.add Ir3.Void q |> k
    | Ir2.INeg e -> helper e (fun () -> Queue.add Ir3.INeg q |> k)
    | Ir2.IAdd (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.IAdd q |> k))
    | Ir2.ISub (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.ISub q |> k))
    | Ir2.IMul (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.IMul q |> k))
    | Ir2.IDiv (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.IDiv q |> k))
    | Ir2.IMod (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.IMod q |> k))
    | Ir2.FNeg e -> helper e (fun () -> Queue.add Ir3.FNeg q |> k)
    | Ir2.FAdd (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.FAdd q |> k))
    | Ir2.FSub (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.FSub q |> k))
    | Ir2.FMul (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.FMul q |> k))
    | Ir2.FDiv (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.FDiv q |> k))
    | Ir2.IEq (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.IEq q |> k))
    | Ir2.INeq (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.INeq q |> k))
    | Ir2.IGt (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.IGt q |> k))
    | Ir2.IGe (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.IGe q |> k))
    | Ir2.ILt (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.ILt q |> k))
    | Ir2.ILe (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.ILe q |> k))
    | Ir2.FGt (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.FGt q |> k))
    | Ir2.FGe (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.FGe q |> k))
    | Ir2.FLt (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.FLt q |> k))
    | Ir2.FLe (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.FLe q |> k))
    | Ir2.CEq (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.CEq q |> k))
    | Ir2.CNeq (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.CNeq q |> k))
    | Ir2.CGt (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.CGt q |> k))
    | Ir2.CGe (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.CGe q |> k))
    | Ir2.CLt (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.CLt q |> k))
    | Ir2.CLe (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.CLe q |> k))
    | Ir2.BEq (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.BEq q |> k))
    | Ir2.Not e -> helper e (fun () -> Queue.add Ir3.Not q |> k)
    | Ir2.And (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.And q |> k))
    | Ir2.Or (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.Or q |> k))
    | Ir2.Xor (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.Xor q |> k))
    | Ir2.List es ->
        let len = Array.length es in
        let rec h es i k =
          if i >= len then () |> k else helper es.(i) (fun () -> h es (i + 1) k)
        in
        h es 0 (fun () -> Queue.add (Ir3.List len) q |> k)
    | Ir2.At (e1, e2) ->
        helper e1 (fun () -> helper e2 (fun () -> Queue.add Ir3.At q |> k))
    | Ir2.Tuple es ->
        let len = List.length es in
        let rec h es k =
          match es with
          | [] -> () |> k
          | e :: es' -> helper e (fun () -> h es' k)
        in
        h es (fun () -> Queue.add (Ir3.Tuple len) q |> k)
    | Ir2.FnVal (ps, c, body) ->
        flatten_expr body (fun body' ->
            Queue.add
              (Ir3.FnVal (ps, Array.of_list c, Queue.length body' + 1))
              q;
            Queue.transfer body' q;
            Queue.add Ir3.JmpBck q;
            () |> k)
    | Ir2.FnCall (fn, arg) ->
        helper fn (fun () -> helper arg (fun () -> Queue.add Ir3.FnCall q |> k))
    | Ir2.FnTailCall (fn, arg) ->
        helper fn (fun () ->
            helper arg (fun () -> Queue.add Ir3.FnTailCall q |> k))
    | Ir2.Bind (name, expr) ->
        flatten_expr expr (fun expr' ->
            Queue.transfer expr' q;
            Queue.add (Ir3.Bind name) q |> k)
    | Ir2.If (expr, body1, body2) ->
        flatten_expr expr (fun expr' ->
            flatten_expr body1 (fun body1' ->
                flatten_expr body2 (fun body2' ->
                    Queue.transfer expr' q;
                    Queue.add Ir3.If q;
                    Queue.add (Ir3.Jmp (Queue.length body1' + 2)) q;
                    Queue.transfer body1' q;
                    Queue.add (Ir3.Jmp (Queue.length body2' + 1)) q;
                    Queue.transfer body2' q;
                    () |> k)))
    | Ir2.Block es -> visit_es es (fun es' -> Queue.transfer es' q |> k)
    | Ir2.Do expr ->
        flatten_expr expr (fun expr' ->
            Queue.transfer expr' q;
            Queue.add Ir3.Pop q |> k)
    | Ir2.Noop -> () |> k
  in
  helper expr (fun () -> q |> k)

let run ds =
  visit_es ds (fun ds' ->
      Array.init (Queue.length ds') (fun _ -> Queue.take ds'))
