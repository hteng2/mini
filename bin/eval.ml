module Vars = Map.Make (String)

let init_scope = Vars.empty

let rec eval (scope : int Vars.t) (expr : Analyzer.expr) : int * int Vars.t =
  match expr with
  | Analyzer.Num i -> (i, scope)
  | Analyzer.Name s -> ((try Vars.find s scope with Not_found -> 0), scope)
  | Analyzer.Assign (n, r) ->
      let v, scope' = eval scope r in
      (v, Vars.add n v scope')
  | Analyzer.Add (l, r) ->
      let vl, scopel = eval scope l in
      let vr, scoper = eval scopel r in
      (vl + vr, scoper)
  | Analyzer.Sub (l, r) ->
      let vl, scopel = eval scope l in
      let vr, scoper = eval scopel r in
      (vl - vr, scoper)
  | Analyzer.Mul (l, r) ->
      let vl, scopel = eval scope l in
      let vr, scoper = eval scopel r in
      (vl * vr, scoper)
  | Analyzer.Div (l, r) ->
      let vl, scopel = eval scope l in
      let vr, scoper = eval scopel r in
      (vl / vr, scoper)
