module Vars = Map.Make (String)

exception Quit

let init_scope = Vars.empty

let runCmd s scope =
  match s with
  | "help" ->
      print_endline "help here";
      None
  | "quit" -> raise Quit
  | _ -> raise (Failure "unimplemented command")

let rec eval (scope : int Vars.t) (expr : Analyzer.expr) =
  match expr with
  | Analyzer.Num i -> Some (i, scope)
  | Analyzer.Name s ->
      Some ((try Vars.find s scope with Not_found -> 0), scope)
  | Analyzer.Cmd s -> runCmd s scope
  | Analyzer.Neg e ->
      Option.bind (eval scope e) (fun (ve, scope') -> Some (-ve, scope'))
  | Analyzer.Assign (n, r) ->
      Option.bind (eval scope r) (fun (v, scope') ->
          Some (v, Vars.add n v scope'))
  | Analyzer.Add (l, r) ->
      Option.bind (eval scope l) (fun (vl, scope') ->
          Option.bind (eval scope' r) (fun (vr, scope'') ->
              Some (vl + vr, scope'')))
  | Analyzer.Sub (l, r) ->
      Option.bind (eval scope l) (fun (vl, scope') ->
          Option.bind (eval scope' r) (fun (vr, scope'') ->
              Some (vl - vr, scope'')))
  | Analyzer.Mul (l, r) ->
      Option.bind (eval scope l) (fun (vl, scope') ->
          Option.bind (eval scope' r) (fun (vr, scope'') ->
              Some (vl * vr, scope'')))
  | Analyzer.Div (l, r) ->
      Option.bind (eval scope l) (fun (vl, scope') ->
          Option.bind (eval scope' r) (fun (vr, scope'') ->
              Some (vl / vr, scope'')))
