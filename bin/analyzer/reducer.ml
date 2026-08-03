open Mini

let rec reduce (ir : Ir.expr) base =
  let pure, ir' =
    match fst ir.v with
    | Ir.Int _ | Ir.Float _ | Ir.Char _ | Ir.Str _ | Ir.Bool _ | Ir.Void ->
        (true, ir)
    | Ir.Name _ -> (false, ir)
    | Ir.Neg e -> (
        let pure, e' = reduce e false in
        if not pure then (false, ir)
        else
          let t = snd e'.v in
          match fst e'.v with
          | Ir.Int n -> (true, { e' with v = (Ir.Int (-n), t) })
          | Ir.Float f -> (true, { e' with v = (Ir.Float (-.f), t) })
          | _ -> (false, ir))
    | Ir.Add (e1, e2) -> (
        let pure1, e1' = reduce e1 false in
        let pure2, e2' = reduce e2 false in
        if (not pure1) || not pure2 then
          (false, { ir with v = (Ir.Add (e1', e2'), snd e1'.v) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir.Int n1, Ir.Int n2 ->
              (true, { ir with v = (Ir.Int (n1 + n2), snd e1'.v) })
          | Ir.Float f1, Ir.Float f2 ->
              (true, { ir with v = (Ir.Float (f1 +. f2), snd e1'.v) })
          | _ -> (false, ir))
    | Ir.Sub (e1, e2) -> (
        let pure1, e1' = reduce e1 false in
        let pure2, e2' = reduce e2 false in
        if (not pure1) || not pure2 then
          (false, { ir with v = (Ir.Sub (e1', e2'), snd e1'.v) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir.Int n1, Ir.Int n2 ->
              (true, { ir with v = (Ir.Int (n1 - n2), snd e1'.v) })
          | Ir.Float f1, Ir.Float f2 ->
              (true, { ir with v = (Ir.Float (f1 -. f2), snd e1'.v) })
          | _ -> (false, ir))
    | Ir.Mul (e1, e2) -> (
        let pure1, e1' = reduce e1 false in
        let pure2, e2' = reduce e2 false in
        if (not pure1) || not pure2 then
          (false, { ir with v = (Ir.Mul (e1', e2'), snd e1'.v) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir.Int n1, Ir.Int n2 ->
              (true, { ir with v = (Ir.Int (n1 * n2), snd e1'.v) })
          | Ir.Float f1, Ir.Float f2 ->
              (true, { ir with v = (Ir.Float (f1 *. f2), snd e1'.v) })
          | _ -> (false, ir))
    | Ir.Div (e1, e2) -> (
        let pure1, e1' = reduce e1 false in
        let pure2, e2' = reduce e2 false in
        if (not pure1) || not pure2 then
          (false, { ir with v = (Ir.Div (e1', e2'), snd e1'.v) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir.Int n1, Ir.Int n2 ->
              (true, { ir with v = (Ir.Int (n1 / n2), snd e1'.v) })
          | Ir.Float f1, Ir.Float f2 ->
              (true, { ir with v = (Ir.Float (f1 /. f2), snd e1'.v) })
          | _ -> (false, ir))
    | Ir.Mod (e1, e2) -> (
        let pure1, e1' = reduce e1 false in
        let pure2, e2' = reduce e2 false in
        if (not pure1) || not pure2 then
          (false, { ir with v = (Ir.Mod (e1', e2'), snd e1'.v) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir.Int n1, Ir.Int n2 ->
              (true, { ir with v = (Ir.Int (n1 mod n2), snd e1'.v) })
          | _ -> (false, ir))
    | Ir.Not e -> (
        let pure, e' = reduce e false in
        if not pure then (false, { ir with v = (Ir.Not e', Types.Bool) })
        else
          match fst e'.v with
          | Ir.Bool b -> (true, { e' with v = (Ir.Bool (not b), Types.Bool) })
          | _ -> (false, ir))
    | Ir.And (e1, e2) -> (
        let pure1, e1' = reduce e1 false in
        let pure2, e2' = reduce e2 false in
        if (not pure1) || not pure2 then
          (false, { ir with v = (Ir.And (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir.Bool b1, Ir.Bool b2 ->
              (true, { ir with v = (Ir.Bool (b1 && b2), Types.Bool) })
          | _ -> (false, ir))
    | Ir.Or (e1, e2) -> (
        let pure1, e1' = reduce e1 false in
        let pure2, e2' = reduce e2 false in
        if (not pure1) || not pure2 then
          (false, { ir with v = (Ir.Or (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir.Bool b1, Ir.Bool b2 ->
              (true, { ir with v = (Ir.Bool (b1 || b2), Types.Bool) })
          | _ -> (false, ir))
    | Ir.Xor (e1, e2) -> (
        let pure1, e1' = reduce e1 false in
        let pure2, e2' = reduce e2 false in
        if (not pure1) || not pure2 then
          (false, { ir with v = (Ir.Xor (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir.Bool b1, Ir.Bool b2 ->
              (true, { ir with v = (Ir.Bool (b1 <> b2), Types.Bool) })
          | _ -> (false, ir))
    | Ir.Eq (e1, e2) -> (
        let pure1, e1' = reduce e1 false in
        let pure2, e2' = reduce e2 false in
        if (not pure1) || not pure2 then
          (false, { ir with v = (Ir.Eq (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir.Bool b1, Ir.Bool b2 ->
              (true, { ir with v = (Ir.Bool (b1 = b2), Types.Bool) })
          | Ir.Int n1, Ir.Int n2 ->
              (true, { ir with v = (Ir.Bool (n1 = n2), Types.Bool) })
          | Ir.Char c1, Ir.Char c2 ->
              (true, { ir with v = (Ir.Bool (c1 = c2), Types.Bool) })
          | _ -> (false, ir))
    | Ir.Neq (e1, e2) -> (
        let pure1, e1' = reduce e1 false in
        let pure2, e2' = reduce e2 false in
        if (not pure1) || not pure2 then
          (false, { ir with v = (Ir.Neq (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir.Bool b1, Ir.Bool b2 ->
              (true, { ir with v = (Ir.Bool (b1 = b2), Types.Bool) })
          | Ir.Int n1, Ir.Int n2 ->
              (true, { ir with v = (Ir.Bool (n1 <> n2), Types.Bool) })
          | Ir.Char c1, Ir.Char c2 ->
              (true, { ir with v = (Ir.Bool (c1 <> c2), Types.Bool) })
          | _ -> (false, ir))
    | Ir.Gt (e1, e2) -> (
        let pure1, e1' = reduce e1 false in
        let pure2, e2' = reduce e2 false in
        if (not pure1) || not pure2 then
          (false, { ir with v = (Ir.Gt (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir.Int n1, Ir.Int n2 ->
              (true, { ir with v = (Ir.Bool (n1 > n2), Types.Bool) })
          | Ir.Float f1, Ir.Float f2 ->
              (true, { ir with v = (Ir.Bool (f1 > f2), Types.Bool) })
          | Ir.Char c1, Ir.Char c2 ->
              (true, { ir with v = (Ir.Bool (c1 > c2), Types.Bool) })
          | _ -> (false, ir))
    | Ir.Ge (e1, e2) -> (
        let pure1, e1' = reduce e1 false in
        let pure2, e2' = reduce e2 false in
        if (not pure1) || not pure2 then
          (false, { ir with v = (Ir.Ge (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir.Int n1, Ir.Int n2 ->
              (true, { ir with v = (Ir.Bool (n1 >= n2), Types.Bool) })
          | Ir.Float f1, Ir.Float f2 ->
              (true, { ir with v = (Ir.Bool (f1 >= f2), Types.Bool) })
          | Ir.Char c1, Ir.Char c2 ->
              (true, { ir with v = (Ir.Bool (c1 >= c2), Types.Bool) })
          | _ -> (false, ir))
    | Ir.Lt (e1, e2) -> (
        let pure1, e1' = reduce e1 false in
        let pure2, e2' = reduce e2 false in
        if (not pure1) || not pure2 then
          (false, { ir with v = (Ir.Lt (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir.Int n1, Ir.Int n2 ->
              (true, { ir with v = (Ir.Bool (n1 < n2), Types.Bool) })
          | Ir.Float f1, Ir.Float f2 ->
              (true, { ir with v = (Ir.Bool (f1 < f2), Types.Bool) })
          | Ir.Char c1, Ir.Char c2 ->
              (true, { ir with v = (Ir.Bool (c1 < c2), Types.Bool) })
          | _ -> (false, ir))
    | Ir.Le (e1, e2) -> (
        let pure1, e1' = reduce e1 false in
        let pure2, e2' = reduce e2 false in
        if (not pure1) || not pure2 then
          (false, { ir with v = (Ir.Le (e1', e2'), Types.Bool) })
        else
          match (fst e1'.v, fst e2'.v) with
          | Ir.Int n1, Ir.Int n2 ->
              (true, { ir with v = (Ir.Bool (n1 <= n2), Types.Bool) })
          | Ir.Float f1, Ir.Float f2 ->
              (true, { ir with v = (Ir.Bool (f1 <= f2), Types.Bool) })
          | Ir.Char c1, Ir.Char c2 ->
              (true, { ir with v = (Ir.Bool (c1 <= c2), Types.Bool) })
          | _ -> (false, ir))
    | Ir.List es ->
        let es' = List.map (fun e -> reduce e false) es in
        let pure = List.for_all fst es' in
        let es'' = List.map snd es' in
        (pure, { ir with v = (Ir.List es'', snd ir.v) })
    | Ir.ListAt (e1, e2) -> (
        let pure1, e1' = reduce e1 false in
        let pure2, e2' = reduce e2 false in
        if (not pure1) || not pure2 then
          (false, { ir with v = (Ir.ListAt (e1', e2'), snd ir.v) })
        else
          match (snd e1'.v, fst e1'.v, fst e2'.v) with
          | Types.List t, Ir.List l, Ir.Int i ->
              (true, { ir with v = (List.nth l i).v })
          | _ -> assert false)
    | Ir.StrAt (e1, e2) -> (
        let pure1, e1' = reduce e1 false in
        let pure2, e2' = reduce e2 false in
        if (not pure1) || not pure2 then
          (false, { ir with v = (Ir.StrAt (e1', e2'), snd ir.v) })
        else
          match (snd e1'.v, fst e1'.v, fst e2'.v) with
          | Types.Str, Ir.Str s1, Ir.Int s2 ->
              (true, { ir with v = (Ir.Char s1.[s2], snd ir.v) })
          | _ -> assert false)
    | Ir.FnVal (args, closure, body) ->
        let pure, body' = reduce body false in
        (false, { ir with v = (Ir.FnVal (args, closure, body'), snd ir.v) })
    | Ir.FnCall (e1, es) ->
        let pure1, e1' = reduce e1 false in
        let es' = List.map (fun e -> reduce e false) es in
        let pure2 = List.for_all fst es' in
        ( pure1 && pure2,
          { ir with v = (Ir.FnCall (e1', List.map snd es'), snd ir.v) } )
    | Ir.Let (s, e) ->
        let pure, e' = reduce e false in
        (pure, { ir with v = (Ir.Let (s, e'), snd ir.v) })
    | Ir.If (e1, e2, e3) ->
        let pure1, e1' = reduce e1 false in
        let pure2, e2' = reduce e2 false in
        let pure3, e3' = reduce e3 false in
        if pure1 then
          match fst e1'.v with
          | Ir.Bool b -> (true, { (if b then e2' else e3') with span = ir.span })
          | _ -> (false, { ir with v = (Ir.If (e1', e2', e3'), snd ir.v) })
        else (false, { ir with v = (Ir.If (e1', e2', e3'), snd ir.v) })
    | Ir.Block es ->
        let es' = Array.map (fun e -> reduce e false) es in
        let pure = Array.for_all fst es' in
        let es'' = Array.map snd es' in
        (pure, { ir with v = (Ir.Block es'', snd ir.v) })
    | Ir.Do e ->
        let pure, e' = reduce e false in
        if pure then (true, { ir with v = (Ir.Noop, snd ir.v) })
        else (false, { ir with v = (Ir.Do e', snd ir.v) })
    | Ir.Noop -> (true, ir)
  in
  if base && pure then
    (true, ({ ir' with v = (Ir.Noop, snd ir.v) } : (Ir.e * Types.t) Loc.spanned))
  else (pure, ir')

let run es = es |> Array.map (fun e -> reduce e true) |> Array.map snd
