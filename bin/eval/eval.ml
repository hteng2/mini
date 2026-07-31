open Mini

exception Div
exception Range

let run_with_scope scopes f = f (Closure.empty () :: scopes)

let rec exec es scopes =
  let vs = Stack.create () in
  let ls = Stack.create () in
  let cs = Stack.create () in
  let rec helper expr i scopes =
    Printf.printf "| %d %d\n" i (Stack.length vs);
    if i >= Array.length expr then ()
    else
      match expr.(i) with
      | Ir2.Int n ->
          Stack.push (Values.Int n) vs;
          helper expr (i + 1) scopes
      | Ir2.Float n ->
          Stack.push (Values.Float n) vs;
          helper expr (i + 1) scopes
      | Ir2.Char c ->
          Stack.push (Values.Char c) vs;
          helper expr (i + 1) scopes
      | Ir2.Str s ->
          Stack.push (Values.Str s) vs;
          helper expr (i + 1) scopes
      | Ir2.Name name ->
          (match Closure.search scopes name with
          | None ->
              print_endline name;
              assert false
          | Some value -> Stack.push (Values.value_to_v value) vs);
          helper expr (i + 1) scopes
      | Ir2.Bool b ->
          Stack.push (Values.Bool b) vs;
          helper expr (i + 1) scopes
      | Ir2.Void ->
          Stack.push Values.Void vs;
          helper expr (i + 1) scopes
      | Ir2.Neg ->
          (let v = Stack.pop vs in
           match v with
           | Values.Int n -> Stack.push (Values.Int (-n)) vs
           | Values.Float n -> Stack.push (Values.Float (Float.neg n)) vs
           | _ -> assert false);
          helper expr (i + 1) scopes
      | Ir2.Not ->
          (let v = Stack.pop vs in
           match v with
           | Values.Bool b -> Stack.push (Values.Bool (not b)) vs
           | _ -> assert false);
          helper expr (i + 1) scopes
      | Ir2.Eq ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Bool v1, Values.Bool v2 ->
               Stack.push (Values.Bool (v1 = v2)) vs
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Bool (v1 = v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scopes
      | Ir2.Neq ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Bool v1, Values.Bool v2 ->
               Stack.push (Values.Bool (v1 <> v2)) vs
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Bool (v1 <> v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scopes
      | Ir2.Gt ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Bool (v1 > v2)) vs
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Bool (v1 > v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scopes
      | Ir2.Ge ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Bool (v1 >= v2)) vs
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Bool (v1 >= v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scopes
      | Ir2.Lt ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Bool (v1 < v2)) vs
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Bool (v1 < v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scopes
      | Ir2.Le ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Bool (v1 <= v2)) vs
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Bool (v1 <= v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scopes
      | Ir2.Add ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Int (v1 + v2)) vs
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Float (Float.add v1 v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scopes
      | Ir2.Sub ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Int (v1 - v2)) vs
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Float (Float.sub v1 v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scopes
      | Ir2.Mul ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Int (v1 * v2)) vs
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Float (Float.mul v1 v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scopes
      | Ir2.Div ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int _, Values.Int 0 -> raise Div
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Int (v1 / v2)) vs
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Float (Float.div v1 v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scopes
      | Ir2.Mod ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int _, Values.Int 0 -> raise Div
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Int (v1 mod v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scopes
      | Ir2.And ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Bool v1, Values.Bool v2 ->
               Stack.push (Values.Bool (v1 && v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scopes
      | Ir2.Or ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Bool v1, Values.Bool v2 ->
               Stack.push (Values.Bool (v1 || v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scopes
      | Ir2.Xor ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Bool v1, Values.Bool v2 ->
               Stack.push (Values.Bool (v1 <> v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scopes
      | Ir2.List len ->
          let a = List.init len (fun _ -> Stack.pop vs) in
          let b = List.rev a in
          let c = Array.of_list b in
          Stack.push (Values.List c) vs;
          helper expr (i + 1) scopes
      | Ir2.ListAt ->
          (let i = Stack.pop vs in
           let l = Stack.pop vs in
           match (l, i) with
           | Values.List l, Values.Int i ->
               if 0 <= i && i < Array.length l then Stack.push l.(i) vs
               else raise Range
           | _ -> assert false);
          helper expr (i + 1) scopes
      | Ir2.StrAt ->
          (let i = Stack.pop vs in
           let s = Stack.pop vs in
           match (s, i) with
           | Values.Str s, Values.Int i ->
               if 0 <= i && i < String.length s then
                 Stack.push (Values.Char s.[i]) vs
               else raise Range
           | _ -> assert false);
          helper expr (i + 1) scopes
      | Ir2.FnVal (ps, c, len) ->
          let c' = Closure.empty () in
          Closure.iter
            (fun name _ ->
              match Closure.search scopes name with
              | None ->
                  print_string "a";
                  Printf.printf "\"%s\"\n" name
              | Some value -> Closure.set c' name value)
            c;
          Stack.push (Values.Fn (ps, c', i + 1)) vs;
          helper expr (i + 1 + len) scopes
      | Ir2.FnCall len -> (
          let args = List.init len (fun _ -> Stack.pop vs) in
          let args' = List.rev args in
          let fn' = Stack.pop vs in
          match fn' with
          | Values.Fn (ps, closure, loc) ->
              List.iter2
                (fun name ->
                  fun arg ->
                   if name = "_" then ()
                   else Closure.set closure name (Values.Var (ref arg)))
                ps args';
              Stack.push scopes cs;
              Stack.push (i + 1) ls;
              helper expr loc [ closure ]
          | Values.Builtin body ->
              Stack.push (body args') vs;
              helper expr (i + 1) scopes
          | _ -> assert false)
      | Ir2.Store (id, n) ->
          exec_set vs id n scopes;
          helper expr (i + 1) scopes
      | Ir2.Let id ->
          let v = Stack.pop vs in
          Closure.set (List.nth scopes 0) id (Values.Const v);
          helper expr (i + 1) scopes
      | Ir2.If -> (
          match Stack.pop vs with
          | Values.Bool true -> helper expr (i + 2) scopes
          | Values.Bool false -> helper expr (i + 1) scopes
          | _ -> assert false)
      | Ir2.Jmp n -> helper expr (i + n) scopes
      | Ir2.Label _ -> assert false
      | Ir2.JmpBck -> helper expr (Stack.pop ls) (Stack.pop cs)
  in
  helper es 0 scopes

and exec_set stack id n scopes =
  let v = Stack.pop stack in
  let idxs =
    List.init n (fun _ -> Stack.pop stack)
    |> List.map (fun v -> match v with Values.Int i -> i | _ -> assert false)
  in
  let base =
    match Closure.search scopes id with
    | Some (Values.Var x) -> x
    | None ->
        let r = ref v in
        Closure.set (List.nth scopes 0) id (Values.Var r);
        r
    | _ -> assert false
  in
  let rec helper base idxs =
    match idxs with
    | [] -> base := v
    | idx :: idxs' -> (
        match !base with
        | Values.List l -> helper (ref l.(idx)) idxs'
        | Values.Str s -> (
            match v with
            | Values.Char char ->
                let s' =
                  String.mapi (fun i c -> if i = idx then char else c) s
                in
                base := Values.Str s'
            | _ -> assert false)
        | _ -> assert false)
  in
  helper base idxs

let run ds =
  let scope = Closure.empty () in
  Builtins.Fns.iter
    (fun name ({ def } : Builtins.builtinFn) ->
      Closure.set scope name (Values.Const (Values.Builtin def)))
    Builtins.builtins;
  exec ds [ scope ];
  flush stdout
