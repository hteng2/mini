open Mini

exception Div
exception Range

let exec es scopes =
  let vs = Stack.create () in
  let ls = Stack.create () in
  let cs = Stack.create () in
  let rec helper expr i scope =
    (* Printf.printf "i = %d; vs = %d; ls = %d; cs = %d\n" i (Stack.length vs)
      (Stack.length ls) (Stack.length cs); *)
    if i >= Array.length expr then ()
    else
      match expr.(i) with
      | Bytecode.Int n ->
          Stack.push (Values.Int n) vs;
          helper expr (i + 1) scope
      | Bytecode.Float n ->
          Stack.push (Values.Float n) vs;
          helper expr (i + 1) scope
      | Bytecode.Char c ->
          Stack.push (Values.Char c) vs;
          helper expr (i + 1) scope
      | Bytecode.Str s ->
          Stack.push (Values.Str s) vs;
          helper expr (i + 1) scope
      | Bytecode.Name name ->
          (match Hashtbl.find_opt scope name with
          | None -> assert false
          | Some value -> Stack.push value vs);
          helper expr (i + 1) scope
      | Bytecode.Bool b ->
          Stack.push (Values.Bool b) vs;
          helper expr (i + 1) scope
      | Bytecode.Void ->
          Stack.push Values.Void vs;
          helper expr (i + 1) scope
      | Bytecode.Neg ->
          (let v = Stack.pop vs in
           match v with
           | Values.Int n -> Stack.push (Values.Int (-n)) vs
           | Values.Float n -> Stack.push (Values.Float (Float.neg n)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Bytecode.Not ->
          (let v = Stack.pop vs in
           match v with
           | Values.Bool b -> Stack.push (Values.Bool (not b)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Bytecode.Eq ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Bool v1, Values.Bool v2 ->
               Stack.push (Values.Bool (v1 = v2)) vs
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Bool (v1 = v2)) vs
           | Values.Char v1, Values.Char v2 ->
               Stack.push (Values.Bool (v1 = v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Bytecode.Neq ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Bool v1, Values.Bool v2 ->
               Stack.push (Values.Bool (v1 <> v2)) vs
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Bool (v1 <> v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Bytecode.Gt ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Bool (v1 > v2)) vs
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Bool (v1 > v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Bytecode.Ge ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Bool (v1 >= v2)) vs
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Bool (v1 >= v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Bytecode.Lt ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Bool (v1 < v2)) vs
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Bool (v1 < v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Bytecode.Le ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Bool (v1 <= v2)) vs
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Bool (v1 <= v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Bytecode.Add ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Int (v1 + v2)) vs
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Float (Float.add v1 v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Bytecode.Sub ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Int (v1 - v2)) vs
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Float (Float.sub v1 v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Bytecode.Mul ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Int (v1 * v2)) vs
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Float (Float.mul v1 v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Bytecode.Div ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int _, Values.Int 0 -> raise Div
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Int (v1 / v2)) vs
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Float (Float.div v1 v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Bytecode.Mod ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int _, Values.Int 0 -> raise Div
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Int (v1 mod v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Bytecode.And ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Bool v1, Values.Bool v2 ->
               Stack.push (Values.Bool (v1 && v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Bytecode.Or ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Bool v1, Values.Bool v2 ->
               Stack.push (Values.Bool (v1 || v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Bytecode.Xor ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Bool v1, Values.Bool v2 ->
               Stack.push (Values.Bool (v1 <> v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Bytecode.List len ->
          let a = List.init len (fun _ -> Stack.pop vs) in
          let b = List.rev a in
          let c = Array.of_list b in
          Stack.push (Values.List c) vs;
          helper expr (i + 1) scope
      | Bytecode.ListAt ->
          (let i = Stack.pop vs in
           let l = Stack.pop vs in
           match (l, i) with
           | Values.List l, Values.Int i ->
               if 0 <= i && i < Array.length l then Stack.push l.(i) vs
               else raise Range
           | _ -> assert false);
          helper expr (i + 1) scope
      | Bytecode.StrAt ->
          (let i = Stack.pop vs in
           let s = Stack.pop vs in
           match (s, i) with
           | Values.Str s, Values.Int i ->
               if 0 <= i && i < String.length s then
                 Stack.push (Values.Char s.[i]) vs
               else raise Range
           | _ -> assert false);
          helper expr (i + 1) scope
      | Bytecode.FnVal (ps, c, self, len) ->
          let c' = Hashtbl.create 0 in
          Array.iter
            (fun name ->
              match Hashtbl.find_opt scope name with
              | None -> assert false
              | Some value -> Hashtbl.add c' name value)
            c;
          Stack.push (Values.Fn (ps, c', self, i + 1)) vs;
          helper expr (i + 1 + len) scope
      | Bytecode.FnCall len -> (
          let args =
            List.init len (fun _ -> Stack.pop vs) |> List.rev |> Array.of_list
          in
          let fn' = Stack.pop vs in
          match fn' with
          | Values.Fn (ps, closure, self, loc) ->
              let () = Hashtbl.add closure self fn' in
              Array.iter2 (fun name arg -> Hashtbl.add closure name arg) ps args;
              Stack.push scope cs;
              Stack.push (i + 1) ls;
              helper expr loc closure
          | Values.Builtin body ->
              Stack.push (body args) vs;
              helper expr (i + 1) scope
          | _ -> assert false)
      | Bytecode.FnTailCall len -> (
          let args =
            List.init len (fun _ -> Stack.pop vs) |> List.rev |> Array.of_list
          in
          let fn' = Stack.pop vs in
          match fn' with
          | Values.Fn (ps, closure, self, loc) ->
              let () = Hashtbl.add closure self fn' in
              Array.iter2 (fun name arg -> Hashtbl.add closure name arg) ps args;
              helper expr loc closure
          | Values.Builtin body ->
              Stack.push (body args) vs;
              helper expr (i + 1) scope
          | _ -> assert false)
      | Bytecode.Bind id ->
          let v = Stack.pop vs in
          Hashtbl.add scope id v;
          helper expr (i + 1) scope
      | Bytecode.If -> (
          match Stack.pop vs with
          | Values.Bool true -> helper expr (i + 2) scope
          | Values.Bool false -> helper expr (i + 1) scope
          | _ -> assert false)
      | Bytecode.Jmp n -> helper expr (i + n) scope
      | Bytecode.JmpBck -> helper expr (Stack.pop ls) (Stack.pop cs)
      | Bytecode.Pop ->
          let _ = Stack.pop vs in
          helper expr (i + 1) scope
  in
  helper es 0 scopes

let run ds =
  let scope = Hashtbl.create 0 in
  List.iteri
    (fun i (bfn : Builtins.builtinFn) ->
      Hashtbl.add scope i (Values.Builtin bfn.def))
    Builtins.builtins;
  exec ds scope;
  flush stdout
