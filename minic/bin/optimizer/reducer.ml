open Minic_lib

type ctx = { exprtail : bool; fntail : bool; fn : bool }

let rec reduce (ir : Ir2.expr) ctx : bool * Ir2.expr =
  let pure, (ir' : Ir2.expr) =
    match ir.v with
    | Ir2.Int _ | Ir2.Float _ | Ir2.Char _ | Ir2.Bool _ | Ir2.Void -> (true, ir)
    | Ir2.Name _ -> (false, ir)
    | Ir2.INeg e -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure, e' = reduce e ctx' in
        if not pure then (false, ir)
        else
          match e'.v with
          | Ir2.Int n -> (true, { e' with v = Ir2.Int (-n) })
          | _ -> assert false)
    | Ir2.IAdd (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.IAdd (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 -> (true, { ir with v = Ir2.Int (n1 + n2) })
          | _ -> assert false)
    | Ir2.ISub (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.ISub (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 -> (true, { ir with v = Ir2.Int (n1 - n2) })
          | _ -> assert false)
    | Ir2.IMul (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.IMul (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 -> (true, { ir with v = Ir2.Int (n1 * n2) })
          | _ -> assert false)
    | Ir2.IDiv (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.IDiv (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 -> (true, { ir with v = Ir2.Int (n1 / n2) })
          | _ -> assert false)
    | Ir2.IMod (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.IMod (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 -> (true, { ir with v = Ir2.Int (n1 mod n2) })
          | _ -> assert false)
    | Ir2.FNeg e -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure, e' = reduce e ctx' in
        if not pure then (false, ir)
        else
          match e'.v with
          | Ir2.Float n -> (true, { e' with v = Ir2.Float (-.n) })
          | _ -> assert false)
    | Ir2.FAdd (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.FAdd (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Float n1, Ir2.Float n2 ->
              (true, { ir with v = Ir2.Float (Float.add n1 n2) })
          | _ -> assert false)
    | Ir2.FSub (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.FSub (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Float n1, Ir2.Float n2 ->
              (true, { ir with v = Ir2.Float (Float.sub n1 n2) })
          | _ -> assert false)
    | Ir2.FMul (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.FMul (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Float n1, Ir2.Float n2 ->
              (true, { ir with v = Ir2.Float (Float.mul n1 n2) })
          | _ -> assert false)
    | Ir2.FDiv (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.FDiv (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Float n1, Ir2.Float n2 ->
              (true, { ir with v = Ir2.Float (Float.div n1 n2) })
          | _ -> assert false)
    | Ir2.Not e -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure, e' = reduce e ctx' in
        if not pure then (false, { ir with v = Ir2.Not e' })
        else
          match e'.v with
          | Ir2.Bool b -> (true, { e' with v = Ir2.Bool (not b) })
          | _ -> assert false)
    | Ir2.And (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.And (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Bool b1, Ir2.Bool b2 ->
              (true, { ir with v = Ir2.Bool (b1 && b2) })
          | _ -> assert false)
    | Ir2.Or (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.Or (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Bool b1, Ir2.Bool b2 ->
              (true, { ir with v = Ir2.Bool (b1 || b2) })
          | _ -> assert false)
    | Ir2.Xor (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.Xor (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Bool b1, Ir2.Bool b2 ->
              (true, { ir with v = Ir2.Bool (b1 <> b2) })
          | _ -> assert false)
    | Ir2.IEq (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.IEq (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 -> (true, { ir with v = Ir2.Bool (n1 = n2) })
          | _ -> assert false)
    | Ir2.CEq (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.CEq (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Char c1, Ir2.Char c2 ->
              (true, { ir with v = Ir2.Bool (c1 = c2) })
          | _ -> assert false)
    | Ir2.BEq (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.BEq (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Bool b1, Ir2.Bool b2 ->
              (true, { ir with v = Ir2.Bool (b1 = b2) })
          | _ -> assert false)
    | Ir2.INeq (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.INeq (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 -> (true, { ir with v = Ir2.Bool (n1 <> n2) })
          | _ -> assert false)
    | Ir2.CNeq (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.CNeq (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Char c1, Ir2.Char c2 ->
              (true, { ir with v = Ir2.Bool (c1 <> c2) })
          | _ -> assert false)
    | Ir2.IGt (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.IGt (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 -> (true, { ir with v = Ir2.Bool (n1 > n2) })
          | _ -> assert false)
    | Ir2.FGt (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.FGt (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Float f1, Ir2.Float f2 ->
              (true, { ir with v = Ir2.Bool (f1 > f2) })
          | _ -> assert false)
    | Ir2.CGt (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.CGt (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Char c1, Ir2.Char c2 ->
              (true, { ir with v = Ir2.Bool (c1 > c2) })
          | _ -> assert false)
    | Ir2.IGe (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.IGe (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 -> (true, { ir with v = Ir2.Bool (n1 >= n2) })
          | _ -> assert false)
    | Ir2.FGe (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.FGe (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Float f1, Ir2.Float f2 ->
              (true, { ir with v = Ir2.Bool (f1 >= f2) })
          | _ -> assert false)
    | Ir2.CGe (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.CGe (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Char c1, Ir2.Char c2 ->
              (true, { ir with v = Ir2.Bool (c1 >= c2) })
          | _ -> assert false)
    | Ir2.ILt (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.ILt (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 -> (true, { ir with v = Ir2.Bool (n1 < n2) })
          | _ -> assert false)
    | Ir2.FLt (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.FLt (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Float f1, Ir2.Float f2 ->
              (true, { ir with v = Ir2.Bool (f1 < f2) })
          | _ -> assert false)
    | Ir2.CLt (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.CLt (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Char c1, Ir2.Char c2 ->
              (true, { ir with v = Ir2.Bool (c1 < c2) })
          | _ -> assert false)
    | Ir2.ILe (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.ILe (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 -> (true, { ir with v = Ir2.Bool (n1 <= n2) })
          | _ -> assert false)
    | Ir2.FLe (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.FLe (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Float f1, Ir2.Float f2 ->
              (true, { ir with v = Ir2.Bool (f1 <= f2) })
          | _ -> assert false)
    | Ir2.CLe (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.CLe (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.Char c1, Ir2.Char c2 ->
              (true, { ir with v = Ir2.Bool (c1 <= c2) })
          | _ -> assert false)
    | Ir2.List es ->
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let es' = Array.map (fun e -> reduce e ctx') es in
        let pure = Array.for_all fst es' in
        let es'' = Array.map snd es' in
        (pure, { ir with v = Ir2.List es'' })
    | Ir2.At (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, { ir with v = Ir2.At (e1', e2') })
        else
          match (e1'.v, e2'.v) with
          | Ir2.List l, Ir2.Int i -> (true, { ir with v = l.(i).v })
          | _ -> assert false)
    | Ir2.Tuple es ->
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let es' = List.map (fun e -> reduce e ctx') es in
        let pure = List.for_all fst es' in
        let es'' = List.map snd es' in
        (pure, { ir with v = Ir2.Tuple es'' })
    | Ir2.FnVal (argc, closure, body) ->
        let ctx' = { exprtail = true; fntail = false; fn = true } in
        let pure, body' = reduce body ctx' in
        (false, { ir with v = Ir2.FnVal (argc, closure, body') })
    | Ir2.FnCall (fn, arg) ->
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, fn' = reduce fn ctx' in
        let pure2, arg' = reduce arg ctx' in
        if ctx.fntail then (false, { ir with v = Ir2.FnTailCall (fn', arg') })
        else (false, { ir with v = Ir2.FnCall (fn', arg') })
    | Ir2.FnTailCall (e1, es) -> assert false
    | Ir2.Bind (s, e) ->
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure, e' = reduce e ctx' in
        (false, { ir with v = Ir2.Bind (s, e') })
    | Ir2.If (e1, e2, e3) ->
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let ctx' = { exprtail = true; fntail = ctx.fntail; fn = ctx.fn } in
        let pure2, e2' = reduce e2 ctx' in
        let pure3, e3' = reduce e3 ctx' in
        if pure1 then
          match e1'.v with
          | Ir2.Bool b ->
              (true, { (if b then e2' else e3') with span = ir.span })
          | _ -> (false, { ir with v = Ir2.If (e1', e2', e3') })
        else (false, { ir with v = Ir2.If (e1', e2', e3') })
    | Ir2.Block es ->
        let es' = reduce_es es ctx.fn in
        let pure = Array.for_all fst es' in
        let es'' = Array.map snd es' in
        (pure, { ir with v = Ir2.Block es'' })
    | Ir2.Do e ->
        let pure, e' = reduce e ctx in
        (pure, { ir with v = Ir2.Do e' })
    | Ir2.Noop -> assert false
  in
  match (ctx.exprtail, pure) with
  | false, true -> (pure, { ir' with v = Ir2.Noop })
  | _, _ -> (pure, ir')

and reduce_es es fn =
  es |> Array.to_seq
  |> Seq.mapi (fun i e ->
      let ctx' =
        {
          exprtail = i = Array.length es - 1;
          fntail = fn && i = Array.length es - 1;
          fn;
        }
      in
      reduce e ctx')
  |> Seq.filter (fun (pure, (ir : Ir2.expr)) -> ir.v != Ir2.Noop)
  |> Array.of_seq

let run es = reduce_es es false |> Array.map snd
