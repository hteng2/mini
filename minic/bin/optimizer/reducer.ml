open Minic_lib

type ctx = { exprtail : bool; fntail : bool; fn : bool }

let rec reduce (ir : Ir2.expr) ctx =
  let pure, stack, ir' =
    match fst ir.v with
    | Ir2.Int _ | Ir2.Float _ | Ir2.Char _ | Ir2.Bool _ | Ir2.Void ->
        (true, true, ir)
    | Ir2.Name _ -> (false, true, ir)
    | Ir2.INeg e -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure, e' = reduce e ctx' in
        if not pure then (false, true, ir)
        else
          let t = snd e'.v in
          match fst e'.v with
          | Ir2.Int n -> (true, true, { e' with v = (Ir2.Int (-n), t) })
          | _ -> assert false)
    | Ir2.IAdd (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.IAdd (e1', e2'), snd e1'.v) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 ->
              (true, true, { ir with v = (Ir2.Int (n1 + n2), snd e1'.v) })
          | _ -> assert false)
    | Ir2.ISub (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.ISub (e1', e2'), snd e1'.v) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 ->
              (true, true, { ir with v = (Ir2.Int (n1 - n2), snd e1'.v) })
          | _ -> assert false)
    | Ir2.IMul (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.IMul (e1', e2'), snd e1'.v) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 ->
              (true, true, { ir with v = (Ir2.Int (n1 * n2), snd e1'.v) })
          | _ -> assert false)
    | Ir2.IDiv (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.IDiv (e1', e2'), snd e1'.v) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 ->
              (true, true, { ir with v = (Ir2.Int (n1 / n2), snd e1'.v) })
          | _ -> assert false)
    | Ir2.IMod (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.IMod (e1', e2'), snd e1'.v) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 ->
              (true, true, { ir with v = (Ir2.Int (n1 mod n2), snd e1'.v) })
          | _ -> assert false)
    | Ir2.FNeg e -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure, e' = reduce e ctx' in
        if not pure then (false, true, ir)
        else
          let t = snd e'.v in
          match fst e'.v with
          | Ir2.Float n -> (true, true, { e' with v = (Ir2.Float (-.n), t) })
          | _ -> assert false)
    | Ir2.FAdd (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.FAdd (e1', e2'), snd e1'.v) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Float n1, Ir2.Float n2 ->
              ( true,
                true,
                { ir with v = (Ir2.Float (Float.add n1 n2), snd e1'.v) } )
          | _ -> assert false)
    | Ir2.FSub (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.FSub (e1', e2'), snd e1'.v) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Float n1, Ir2.Float n2 ->
              ( true,
                true,
                { ir with v = (Ir2.Float (Float.sub n1 n2), snd e1'.v) } )
          | _ -> assert false)
    | Ir2.FMul (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.FMul (e1', e2'), snd e1'.v) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Float n1, Ir2.Float n2 ->
              ( true,
                true,
                { ir with v = (Ir2.Float (Float.mul n1 n2), snd e1'.v) } )
          | _ -> assert false)
    | Ir2.FDiv (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.FDiv (e1', e2'), snd e1'.v) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Float n1, Ir2.Float n2 ->
              ( true,
                true,
                { ir with v = (Ir2.Float (Float.div n1 n2), snd e1'.v) } )
          | _ -> assert false)
    | Ir2.Not e -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure, e' = reduce e ctx' in
        if not pure then (false, true, { ir with v = (Ir2.Not e', Types.Bool) })
        else
          match fst e'.v with
          | Ir2.Bool b ->
              (true, true, { e' with v = (Ir2.Bool (not b), Types.Bool) })
          | _ -> assert false)
    | Ir2.And (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.And (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Bool b1, Ir2.Bool b2 ->
              (true, true, { ir with v = (Ir2.Bool (b1 && b2), Types.Bool) })
          | _ -> assert false)
    | Ir2.Or (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.Or (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Bool b1, Ir2.Bool b2 ->
              (true, true, { ir with v = (Ir2.Bool (b1 || b2), Types.Bool) })
          | _ -> assert false)
    | Ir2.Xor (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.Xor (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Bool b1, Ir2.Bool b2 ->
              (true, true, { ir with v = (Ir2.Bool (b1 <> b2), Types.Bool) })
          | _ -> assert false)
    | Ir2.IEq (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.IEq (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 ->
              (true, true, { ir with v = (Ir2.Bool (n1 = n2), Types.Bool) })
          | _ -> assert false)
    | Ir2.CEq (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.CEq (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Char c1, Ir2.Char c2 ->
              (true, true, { ir with v = (Ir2.Bool (c1 = c2), Types.Bool) })
          | _ -> assert false)
    | Ir2.BEq (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.BEq (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Bool b1, Ir2.Bool b2 ->
              (true, true, { ir with v = (Ir2.Bool (b1 = b2), Types.Bool) })
          | _ -> assert false)
    | Ir2.INeq (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.INeq (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 ->
              (true, true, { ir with v = (Ir2.Bool (n1 <> n2), Types.Bool) })
          | _ -> assert false)
    | Ir2.CNeq (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.CNeq (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Char c1, Ir2.Char c2 ->
              (true, true, { ir with v = (Ir2.Bool (c1 <> c2), Types.Bool) })
          | _ -> assert false)
    | Ir2.IGt (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.IGt (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 ->
              (true, true, { ir with v = (Ir2.Bool (n1 > n2), Types.Bool) })
          | _ -> assert false)
    | Ir2.FGt (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.FGt (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Float f1, Ir2.Float f2 ->
              (true, true, { ir with v = (Ir2.Bool (f1 > f2), Types.Bool) })
          | _ -> assert false)
    | Ir2.CGt (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.CGt (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Char c1, Ir2.Char c2 ->
              (true, true, { ir with v = (Ir2.Bool (c1 > c2), Types.Bool) })
          | _ -> assert false)
    | Ir2.IGe (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.IGe (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 ->
              (true, true, { ir with v = (Ir2.Bool (n1 >= n2), Types.Bool) })
          | _ -> assert false)
    | Ir2.FGe (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.FGe (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Float f1, Ir2.Float f2 ->
              (true, true, { ir with v = (Ir2.Bool (f1 >= f2), Types.Bool) })
          | _ -> assert false)
    | Ir2.CGe (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.CGe (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Char c1, Ir2.Char c2 ->
              (true, true, { ir with v = (Ir2.Bool (c1 >= c2), Types.Bool) })
          | _ -> assert false)
    | Ir2.ILt (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.ILt (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 ->
              (true, true, { ir with v = (Ir2.Bool (n1 < n2), Types.Bool) })
          | _ -> assert false)
    | Ir2.FLt (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.FLt (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Float f1, Ir2.Float f2 ->
              (true, true, { ir with v = (Ir2.Bool (f1 < f2), Types.Bool) })
          | _ -> assert false)
    | Ir2.CLt (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.CLt (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Char c1, Ir2.Char c2 ->
              (true, true, { ir with v = (Ir2.Bool (c1 < c2), Types.Bool) })
          | _ -> assert false)
    | Ir2.ILe (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.ILe (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Int n1, Ir2.Int n2 ->
              (true, true, { ir with v = (Ir2.Bool (n1 <= n2), Types.Bool) })
          | _ -> assert false)
    | Ir2.FLe (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.FLe (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Float f1, Ir2.Float f2 ->
              (true, true, { ir with v = (Ir2.Bool (f1 <= f2), Types.Bool) })
          | _ -> assert false)
    | Ir2.CLe (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.CLe (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir2.Char c1, Ir2.Char c2 ->
              (true, true, { ir with v = (Ir2.Bool (c1 <= c2), Types.Bool) })
          | _ -> assert false)
    | Ir2.List es ->
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let es' = Array.map (fun e -> reduce e ctx') es in
        let pure = Array.for_all fst es' in
        let es'' = Array.map snd es' in
        (pure, true, { ir with v = (Ir2.List es'', snd ir.v) })
    | Ir2.At (e1, e2) -> (
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let pure2, e2' = reduce e2 ctx' in
        if (not pure1) || not pure2 then
          (false, true, { ir with v = (Ir2.At (e1', e2'), snd ir.v) })
        else
          match (snd e1'.v, fst e1'.v, fst e2'.v) with
          | Types.List t, Ir2.List l, Ir2.Int i ->
              (true, true, { ir with v = l.(i).v })
          | _ -> assert false)
    | Ir2.FnVal (argc, closure, body) ->
        let ctx' = { exprtail = true; fntail = false; fn = true } in
        let pure, body' = reduce body ctx' in
        ( false,
          true,
          { ir with v = (Ir2.FnVal (argc, closure, body'), snd ir.v) } )
    | Ir2.FnCall (e1, es) ->
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let es' = List.map (fun e -> reduce e ctx') es in
        let _ = List.for_all fst es' in
        if ctx.fntail then
          ( false,
            true,
            { ir with v = (Ir2.FnTailCall (e1', List.map snd es'), snd ir.v) }
          )
        else
          ( false,
            true,
            { ir with v = (Ir2.FnCall (e1', List.map snd es'), snd ir.v) } )
    | Ir2.FnTailCall (e1, es) -> assert false
    | Ir2.Bind (s, e) ->
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure, e' = reduce e ctx' in
        (false, false, { ir with v = (Ir2.Bind (s, e'), snd ir.v) })
    | Ir2.If (e1, e2, e3) ->
        let ctx' = { exprtail = true; fntail = false; fn = false } in
        let pure1, e1' = reduce e1 ctx' in
        let ctx' = { exprtail = true; fntail = ctx.fntail; fn = ctx.fn } in
        let pure2, e2' = reduce e2 ctx' in
        let pure3, e3' = reduce e3 ctx' in
        if pure1 then
          match fst e1'.v with
          | Ir2.Bool b ->
              (true, true, { (if b then e2' else e3') with span = ir.span })
          | _ ->
              (false, true, { ir with v = (Ir2.If (e1', e2', e3'), snd ir.v) })
        else (false, true, { ir with v = (Ir2.If (e1', e2', e3'), snd ir.v) })
    | Ir2.Block es ->
        let es' =
          Array.mapi
            (fun i e ->
              let ctx' =
                {
                  exprtail = i = Array.length es - 1;
                  fntail = ctx.fn && i = Array.length es - 1;
                  fn = ctx.fn;
                }
              in
              reduce e ctx')
            es
        in
        let pure = Array.for_all fst es' in
        let es'' = Array.map snd es' in
        (pure, true, { ir with v = (Ir2.Block es'', snd ir.v) })
    | Ir2.Do e -> assert false
    | Ir2.Noop -> assert false
  in
  match (ctx.exprtail, pure, stack) with
  | false, true, false ->
      ( pure,
        ({ ir' with v = (Ir2.Noop, snd ir.v) } : (Ir2.e * Types.t) Loc.spanned)
      )
  | false, _, true -> (pure, { ir with v = (Ir2.Do ir', snd ir.v) })
  | _, _, _ -> (pure, ir')

let run es =
  es
  |> Array.mapi (fun i e ->
      reduce e
        { exprtail = i = Array.length es - 1; fntail = false; fn = false })
  |> Array.map snd
