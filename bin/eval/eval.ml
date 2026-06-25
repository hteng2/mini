open Mini

exception Div
exception Range

type 'a cont = {
  break : unit -> 'a;
  continue : unit -> 'a;
  normal : unit -> 'a;
  return : Values.v -> 'a;
}

let run_with_scope scopes f = f (Closure.empty () :: scopes)

let rec eval ds scopes cont =
  let rec helper ds =
    match Queue.take_opt ds with
    | None -> () |> cont.normal
    | Some d -> eval_dec d scopes { cont with normal = (fun () -> helper ds) }
  in
  helper (Queue.copy ds)

and eval_dec : 'a. Ir.dec -> Values.value Closure.t list -> 'a cont -> 'a =
 fun d scopes cont ->
  match d with
  | Ir.Let (name, expr) ->
      (let v = eval_expr expr scopes in
       if name = "_" then ()
       else Closure.set (List.nth scopes 0) name (Values.Const v))
      |> cont.normal
  | Ir.Var (name, expr) ->
      (let v = eval_expr expr scopes in
       if name = "_" then ()
       else Closure.set (List.nth scopes 0) name (Values.Var (ref v)))
      |> cont.normal
  | Ir.VarSet (id, expr) -> eval_varset id expr scopes |> cont.normal
  | Ir.If (expr, body1, body2) -> (
      let v = eval_expr expr scopes in
      match (v, body2) with
      | Values.Bool true, _ ->
          run_with_scope scopes (fun scopes' -> eval_dec body1 scopes' cont)
      | Values.Bool false, Some body2 ->
          run_with_scope scopes (fun scopes' -> eval_dec body2 scopes' cont)
      | _ -> cont.normal ())
  | Ir.While (expr, body) ->
      let rec run () =
        match eval_expr expr scopes with
        | Values.Bool true ->
            run_with_scope scopes (fun scopes' ->
                eval_dec body scopes'
                  {
                    break = cont.normal;
                    continue = run;
                    normal = run;
                    return = cont.return;
                  })
        | _ -> cont.normal ()
      in
      run ()
  | Ir.Break -> () |> cont.break
  | Ir.Continue -> () |> cont.continue
  | Ir.Return e -> eval_expr e scopes |> cont.return
  | Ir.Block p -> run_with_scope scopes (fun scopes' -> eval p scopes' cont)

and eval_expr expr scopes =
  match expr with
  | Ir.Num n -> Values.Int n
  | Ir.Char c -> Values.Char c
  | Ir.Str s -> Values.Str s
  | Ir.Name name -> (
      match Closure.search scopes name with
      | None ->
          print_endline name;
          exit 0
      | Some value -> Values.value_to_v value)
  | Ir.True -> Values.Bool true
  | Ir.False -> Values.Bool false
  | Ir.Void -> Values.Void
  | Ir.Neg expr' -> (
      match eval_expr expr' scopes with
      | Values.Int n -> Values.Int (-n)
      | _ -> assert false)
  | Ir.Pos expr' -> (
      match eval_expr expr' scopes with
      | Values.Int n -> Values.Int n
      | _ -> assert false)
  | Ir.Not expr' -> (
      match eval_expr expr' scopes with
      | Values.Bool b -> Values.Bool (not b)
      | _ -> assert false)
  | Ir.Eq (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Bool v1, Values.Bool v2 -> Values.Bool (v1 = v2)
      | Values.Int v1, Values.Int v2 -> Values.Bool (v1 = v2)
      | _ -> assert false)
  | Ir.Gt (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Int v1, Values.Int v2 -> Values.Bool (v1 > v2)
      | _ -> assert false)
  | Ir.Lt (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Int v1, Values.Int v2 -> Values.Bool (v1 < v2)
      | _ -> assert false)
  | Ir.Add (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 + v2)
      | _ -> assert false)
  | Ir.Sub (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 - v2)
      | _ -> assert false)
  | Ir.Mul (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 * v2)
      | _ -> assert false)
  | Ir.Div (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Int v1, Values.Int 0 -> raise Div
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 / v2)
      | _ -> assert false)
  | Ir.Mod (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Int v1, Values.Int 0 -> raise Div
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 mod v2)
      | _ -> assert false)
  | Ir.And (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Bool v1, Values.Bool v2 -> Values.Bool (v1 && v2)
      | _ -> assert false)
  | Ir.Or (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Bool v1, Values.Bool v2 -> Values.Bool (v1 || v2)
      | _ -> assert false)
  | Ir.Xor (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Bool v1, Values.Bool v2 -> Values.Bool (v1 <> v2)
      | _ -> assert false)
  | Ir.List es ->
      let vs = List.map (fun e -> eval_expr e scopes) es in
      Values.List (Array.of_list vs)
  | Ir.At (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.List l, Values.Int i ->
          if 0 <= i && i < Array.length l then Array.get l i else raise Range
      | Values.Str s, Values.Int i ->
          if 0 <= i && i < String.length s then Values.Char (String.get s i)
          else raise Range
      | _ -> assert false)
  | Ir.FnVal (ps, c, body) ->
      let c' = Closure.empty () in
      Closure.iter
        (fun name _ ->
          match Closure.search scopes name with
          | None ->
              print_string "a";
              Printf.printf "\"%s\"\n" name
          | Some value -> Closure.set c' name value)
        c;
      Values.Fn (ps, c', body)
  | Ir.FnCall (fn, args) -> (
      let fn' = eval_expr fn scopes in
      let args' = List.map (fun arg -> eval_expr arg scopes) args in
      match fn' with
      | Values.Fn (ps, closure, body) ->
          List.iter2
            (fun name ->
              fun arg ->
               if name = "_" then ()
               else Closure.set closure name (Values.Var (ref arg)))
            ps args';
          eval_dec body [ closure ]
            {
              break = (fun () -> assert false);
              continue = (fun () -> assert false);
              normal =
                (fun () ->
                  Debug.print_dec body 0;
                  assert false);
              return = (fun v -> v);
            }
      | Values.Builtin body -> body args'
      | _ -> assert false)

and eval_varset id e scopes =
  let rec helper id =
    match id with
    | Ir.IdName name -> (
        match Closure.search scopes name with
        | Some (Values.Var x) -> x
        | _ -> assert false)
    | Ir.IdAt (id', i) -> (
        let id'' = helper id' in
        let i' = eval_expr i scopes in
        match (!id'', i') with
        | Values.List l, Values.Int i -> ref l.(i)
        | _ -> assert false)
  in
  match id with
  | Ir.IdName _ ->
      let v = eval_expr e scopes in
      helper id := v
  | Ir.IdAt (id', i) -> (
      let r = helper id' in
      let i' = eval_expr i scopes in
      let v = eval_expr e scopes in
      match (!r, i', v) with
      | Values.List l, Values.Int n, _ -> l.(n) <- v
      | Values.Str s, Values.Int n, Values.Char c ->
          let bytes = String.to_bytes s in
          let s' =
            Bytes.set bytes n c;
            Bytes.to_string bytes
          in
          r := Values.Str s'
      | _ -> assert false)

let run ds =
  let scope = Closure.empty () in
  Builtins.Fns.iter
    (fun name ({ def } : Builtins.builtinFn) ->
      Closure.set scope name (Values.Const (Values.Builtin def)))
    Builtins.builtins;
  eval ds [ scope ]
    {
      break = (fun () -> assert false);
      continue = (fun () -> assert false);
      normal = (fun () -> ());
      return = (fun v -> assert false);
    }
