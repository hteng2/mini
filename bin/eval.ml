module Vars = Map.Make (String)

let init_scope =
  let s0 = Vars.empty in
  let s1 = Vars.add "true" 1 s0 in
  let s2 = Vars.add "false" 0 s1 in
  s2

let rec eval (scope : int Vars.t) (expr : Analyzer.expr) : int * int Vars.t =
  match expr with
  | Analyzer.Atom i -> (i, scope)
  | Analyzer.Var s -> ((try Vars.find s scope with Not_found -> 0), scope)
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
