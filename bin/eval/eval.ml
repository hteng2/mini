open Mini

exception Div
exception Range

type 'a flow =
  | Fail
  | Next of (Ir2.dec Array.t * int * Values.value Closure.t list * 'a cont)
  | Do of (unit -> 'a)

and 'a ret = Fail | Val of (Values.v -> 'a)

and 'a cont = {
  break : 'a flow;
  continue : 'a flow;
  next : 'a flow;
  return : 'a ret;
}

let run_with_scope scopes f = f (Closure.empty () :: scopes)

let rec do_flow : 'a flow -> 'a = function
  | Fail -> assert false
  | Next (ds, i, scopes, cont) -> exec ds i scopes cont
  | Do f -> f ()

and do_ret v : 'a ret -> 'a = function Fail -> assert false | Val f -> f v

and exec ds i scopes cont =
  if Array.length ds = i then do_flow cont.next
  else
    exec_dec ds.(i) scopes { cont with next = Next (ds, i + 1, scopes, cont) }

and exec_dec : 'a. Ir2.dec -> Values.value Closure.t list -> 'a cont -> 'a =
 fun d scopes cont ->
  match d with
  | Ir2.Let (name, expr) ->
      let v = eval_expr expr scopes in
      if name <> "_" then Closure.set (List.nth scopes 0) name (Values.Const v);
      do_flow cont.next
  | Ir2.Var (name, expr) ->
      let v = eval_expr expr scopes in
      if name <> "_" then
        Closure.set (List.nth scopes 0) name (Values.Var (ref v));
      do_flow cont.next
  | Ir2.VarSet (id, expr) -> exec_varset id expr scopes cont.next
  | Ir2.If (expr, body1, body2) -> (
      let v = eval_expr expr scopes in
      match (v, body2) with
      | Values.Bool true, _ ->
          run_with_scope scopes (fun scopes' -> exec_dec body1 scopes' cont)
      | Values.Bool false, Some body2 ->
          run_with_scope scopes (fun scopes' -> exec_dec body2 scopes' cont)
      | _ -> do_flow cont.next)
  | Ir2.While (expr, body) ->
      let rec run () =
        match eval_expr expr scopes with
        | Values.Bool true ->
            run_with_scope scopes (fun scopes' ->
                exec_dec body scopes'
                  {
                    break = cont.next;
                    continue = Do run;
                    next = Do run;
                    return = cont.return;
                  })
        | _ -> do_flow cont.next
      in
      do_flow (Do run)
  | Ir2.Break -> do_flow cont.break
  | Ir2.Continue -> do_flow cont.continue
  | Ir2.Return e -> do_ret (eval_expr e scopes) cont.return
  | Ir2.Block p -> run_with_scope scopes (fun scopes' -> exec p 0 scopes' cont)

and exec_varset id e scopes k =
  let rec helper id =
    match id with
    | Ir2.IdName name -> (
        match Closure.search scopes name with
        | Some (Values.Var x) -> x
        | _ -> assert false)
    | Ir2.IdAt (id', i) -> (
        let id'' = helper id' in
        let i' = eval_expr i scopes in
        match (!id'', i') with
        | Values.List l, Values.Int i -> ref l.(i)
        | _ -> assert false)
  in
  match id with
  | Ir2.IdName _ ->
      let v = eval_expr e scopes in
      helper id := v;
      do_flow k
  | Ir2.IdAt (id', i) ->
      (let r = helper id' in
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
       | _ -> assert false);
      do_flow k

and eval_expr expr scopes =
  let vs = Stack.create () in
  let rec helper expr =
    match expr with
    | Ir2.Int n -> Stack.push (Values.Int n) vs
    | Ir2.Float n -> Stack.push (Values.Float n) vs
    | Ir2.Char c -> Stack.push (Values.Char c) vs
    | Ir2.Str s -> Stack.push (Values.Str s) vs
    | Ir2.Name name -> (
        match Closure.search scopes name with
        | None ->
            print_endline name;
            assert false
        | Some value -> Stack.push (Values.value_to_v value) vs)
    | Ir2.Bool b -> Stack.push (Values.Bool b) vs
    | Ir2.Void -> Stack.push Values.Void vs
    | Ir2.Neg -> (
        let v = Stack.pop vs in
        match v with
        | Values.Int n -> Stack.push (Values.Int (-n)) vs
        | Values.Float n -> Stack.push (Values.Float (Float.neg n)) vs
        | _ -> assert false)
    | Ir2.Not -> (
        let v = Stack.pop vs in
        match v with
        | Values.Bool b -> Stack.push (Values.Bool (not b)) vs
        | _ -> assert false)
    | Ir2.Eq -> (
        let v2 = Stack.pop vs in
        let v1 = Stack.pop vs in
        match (v1, v2) with
        | Values.Bool v1, Values.Bool v2 ->
            Stack.push (Values.Bool (v1 = v2)) vs
        | Values.Int v1, Values.Int v2 -> Stack.push (Values.Bool (v1 = v2)) vs
        | _ -> assert false)
    | Ir2.Gt -> (
        let v2 = Stack.pop vs in
        let v1 = Stack.pop vs in
        match (v1, v2) with
        | Values.Int v1, Values.Int v2 -> Stack.push (Values.Bool (v1 > v2)) vs
        | Values.Float v1, Values.Float v2 ->
            Stack.push (Values.Bool (v1 > v2)) vs
        | _ -> assert false)
    | Ir2.Lt -> (
        let v2 = Stack.pop vs in
        let v1 = Stack.pop vs in
        match (v1, v2) with
        | Values.Int v1, Values.Int v2 -> Stack.push (Values.Bool (v1 < v2)) vs
        | Values.Float v1, Values.Float v2 ->
            Stack.push (Values.Bool (v1 < v2)) vs
        | _ -> assert false)
    | Ir2.Add -> (
        let v2 = Stack.pop vs in
        let v1 = Stack.pop vs in
        match (v1, v2) with
        | Values.Int v1, Values.Int v2 -> Stack.push (Values.Int (v1 + v2)) vs
        | Values.Float v1, Values.Float v2 ->
            Stack.push (Values.Float (Float.add v1 v2)) vs
        | _ -> assert false)
    | Ir2.Sub -> (
        let v2 = Stack.pop vs in
        let v1 = Stack.pop vs in
        match (v1, v2) with
        | Values.Int v1, Values.Int v2 -> Stack.push (Values.Int (v1 - v2)) vs
        | Values.Float v1, Values.Float v2 ->
            Stack.push (Values.Float (Float.sub v1 v2)) vs
        | _ -> assert false)
    | Ir2.Mul -> (
        let v2 = Stack.pop vs in
        let v1 = Stack.pop vs in
        match (v1, v2) with
        | Values.Int v1, Values.Int v2 -> Stack.push (Values.Int (v1 * v2)) vs
        | Values.Float v1, Values.Float v2 ->
            Stack.push (Values.Float (Float.mul v1 v2)) vs
        | _ -> assert false)
    | Ir2.Div -> (
        let v2 = Stack.pop vs in
        let v1 = Stack.pop vs in
        match (v1, v2) with
        | Values.Int _, Values.Int 0 -> raise Div
        | Values.Int v1, Values.Int v2 -> Stack.push (Values.Int (v1 / v2)) vs
        | Values.Float v1, Values.Float v2 ->
            Stack.push (Values.Float (Float.div v1 v2)) vs
        | _ -> assert false)
    | Ir2.Mod -> (
        let v2 = Stack.pop vs in
        let v1 = Stack.pop vs in
        match (v1, v2) with
        | Values.Int _, Values.Int 0 -> raise Div
        | Values.Int v1, Values.Int v2 -> Stack.push (Values.Int (v1 mod v2)) vs
        | _ -> assert false)
    | Ir2.And -> (
        let v2 = Stack.pop vs in
        let v1 = Stack.pop vs in
        match (v1, v2) with
        | Values.Bool v1, Values.Bool v2 ->
            Stack.push (Values.Bool (v1 && v2)) vs
        | _ -> assert false)
    | Ir2.Or -> (
        let v2 = Stack.pop vs in
        let v1 = Stack.pop vs in
        match (v1, v2) with
        | Values.Bool v1, Values.Bool v2 ->
            Stack.push (Values.Bool (v1 || v2)) vs
        | _ -> assert false)
    | Ir2.Xor -> (
        let v2 = Stack.pop vs in
        let v1 = Stack.pop vs in
        match (v1, v2) with
        | Values.Bool v1, Values.Bool v2 ->
            Stack.push (Values.Bool (v1 <> v2)) vs
        | _ -> assert false)
    | Ir2.List len ->
        let a = List.init len (fun _ -> Stack.pop vs) in
        let b = List.rev a in
        let c = Array.of_list b in
        Stack.push (Values.List c) vs
    | Ir2.ListAt -> (
        let i = Stack.pop vs in
        let l = Stack.pop vs in
        match (l, i) with
        | Values.List l, Values.Int i ->
            if 0 <= i && i < Array.length l then Stack.push l.(i) vs
            else raise Range
        | _ -> assert false)
    | Ir2.StrAt -> (
        let i = Stack.pop vs in
        let s = Stack.pop vs in
        match (s, i) with
        | Values.Str s, Values.Int i ->
            if 0 <= i && i < String.length s then
              Stack.push (Values.Char s.[i]) vs
            else raise Range
        | _ -> assert false)
    | Ir2.FnVal (ps, c, body) ->
        let c' = Closure.empty () in
        Closure.iter
          (fun name _ ->
            match Closure.search scopes name with
            | None ->
                print_string "a";
                Printf.printf "\"%s\"\n" name
            | Some value -> Closure.set c' name value)
          c;
        Stack.push (Values.Fn (ps, c', body)) vs
    | Ir2.FnCall len -> (
        let args = List.init len (fun _ -> Stack.pop vs) in
        let args' = List.rev args in
        let fn' = Stack.pop vs in
        match fn' with
        | Values.Fn (ps, closure, body) ->
            List.iter2
              (fun name ->
                fun arg ->
                 if name = "_" then ()
                 else Closure.set closure name (Values.Var (ref arg)))
              ps args';
            exec_dec body [ closure ]
              {
                break = Fail;
                continue = Fail;
                next = Fail;
                return = Val (fun v -> Stack.push v vs);
              }
        | Values.Builtin body -> Stack.push (body args') vs
        | _ -> assert false)
  in
  Array.iter helper expr;
  Stack.pop vs

let run ds =
  let scope = Closure.empty () in
  Builtins.Fns.iter
    (fun name ({ def } : Builtins.builtinFn) ->
      Closure.set scope name (Values.Const (Values.Builtin def)))
    Builtins.builtins;
  exec ds 0 [ scope ]
    { break = Fail; continue = Fail; next = Do (fun () -> ()); return = Fail }
