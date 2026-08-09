open Minic_lib

exception Div
exception Range

let exec es scope =
  let vs = Stack.create () in
  let ls = Stack.create () in
  let cs = Stack.create () in
  let rec helper expr i scope =
    (* Printf.printf "i = %d; vs = %d; ls = %d; cs = %d\n" i (Stack.length vs)
      (Stack.length ls) (Stack.length cs); *)
    if i >= Array.length expr then ()
    else
      match expr.(i) with
      (* atoms *)
      | Ir3.Int n ->
          Stack.push (Values.Int n) vs;
          helper expr (i + 1) scope
      | Ir3.Float n ->
          Stack.push (Values.Float n) vs;
          helper expr (i + 1) scope
      | Ir3.Char c ->
          Stack.push (Values.Char c) vs;
          helper expr (i + 1) scope
      | Ir3.Name name ->
          (match Hashtbl.find_opt scope name with
          | None -> assert false
          | Some value -> Stack.push value vs);
          helper expr (i + 1) scope
      | Ir3.Bool b ->
          Stack.push (Values.Bool b) vs;
          helper expr (i + 1) scope
      | Ir3.Void ->
          Stack.push Values.Void vs;
          helper expr (i + 1) scope
      (* arith *)
      (* int *)
      | Ir3.INeg ->
          (let v = Stack.pop vs in
           match v with
           | Values.Int n -> Stack.push (Values.Int (-n)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.IAdd ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Int (v1 + v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.ISub ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Int (v1 - v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.IMul ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Int (v1 * v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.IDiv ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int _, Values.Int 0 -> raise Div
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Int (v1 / v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.IMod ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int _, Values.Int 0 -> raise Div
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Int (v1 mod v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      (* float *)
      | Ir3.FNeg ->
          (let v = Stack.pop vs in
           match v with
           | Values.Float n -> Stack.push (Values.Float (-.n)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.FAdd ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Float (v1 +. v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.FSub ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Float (v1 -. v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.FMul ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Float (v1 *. v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.FDiv ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Float (v1 /. v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      (* logic *)
      | Ir3.Not ->
          (let v = Stack.pop vs in
           match v with
           | Values.Bool b -> Stack.push (Values.Bool (not b)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.And ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Bool v1, Values.Bool v2 ->
               Stack.push (Values.Bool (v1 && v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.Or ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Bool v1, Values.Bool v2 ->
               Stack.push (Values.Bool (v1 || v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.Xor ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Bool v1, Values.Bool v2 ->
               Stack.push (Values.Bool (v1 <> v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      (* comp *)
      (* int *)
      | Ir3.IEq ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Bool (v1 = v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.INeq ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Bool (v1 <> v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.IGt ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Bool (v1 > v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.IGe ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Bool (v1 >= v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.ILt ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Bool (v1 < v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.ILe ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Int v1, Values.Int v2 ->
               Stack.push (Values.Bool (v1 <= v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      (* float *)
      | Ir3.FGt ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Bool (v1 > v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.FGe ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Bool (v1 >= v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.FLt ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Bool (v1 < v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.FLe ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Float v1, Values.Float v2 ->
               Stack.push (Values.Bool (v1 <= v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      (* char *)
      | Ir3.CEq ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Char v1, Values.Char v2 ->
               Stack.push (Values.Bool (v1 = v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.CNeq ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Char v1, Values.Char v2 ->
               Stack.push (Values.Bool (v1 <> v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.CGt ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Char v1, Values.Char v2 ->
               Stack.push (Values.Bool (v1 > v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.CGe ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Char v1, Values.Char v2 ->
               Stack.push (Values.Bool (v1 >= v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.CLt ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Char v1, Values.Char v2 ->
               Stack.push (Values.Bool (v1 < v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      | Ir3.CLe ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Char v1, Values.Char v2 ->
               Stack.push (Values.Bool (v1 <= v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      (* bool *)
      | Ir3.BEq ->
          (let v2 = Stack.pop vs in
           let v1 = Stack.pop vs in
           match (v1, v2) with
           | Values.Bool v1, Values.Bool v2 ->
               Stack.push (Values.Bool (v1 = v2)) vs
           | _ -> assert false);
          helper expr (i + 1) scope
      (* lists *)
      | Ir3.List len ->
          let a = List.init len (fun _ -> Stack.pop vs) in
          let b = List.rev a in
          let c = Array.of_list b in
          Stack.push (Values.List c) vs;
          helper expr (i + 1) scope
      | Ir3.At ->
          (let i = Stack.pop vs in
           let l = Stack.pop vs in
           match (l, i) with
           | Values.List l, Values.Int i ->
               if 0 <= i && i < Array.length l then Stack.push l.(i) vs
               else raise Range
           | _ -> assert false);
          helper expr (i + 1) scope
      (* tuples *)
      | Ir3.Tuple len ->
          let a = List.init len (fun _ -> Stack.pop vs) in
          let b = List.rev a in
          Stack.push (Values.Tuple b) vs;
          helper expr (i + 1) scope
      (* function *)
      | Ir3.FnVal (ps, c, len) ->
          let c' = Array.map (fun name -> Hashtbl.find scope name) c in
          Stack.push (Values.Fn (c', i + 1)) vs;
          helper expr (i + 1 + len) scope
      | Ir3.FnCall -> (
          let arg = Stack.pop vs in
          let fn = Stack.pop vs in
          match (arg, fn) with
          | _, Values.Fn (closure, loc) ->
              let scope' = Hashtbl.create 0 in
              (match arg with
              | Values.Tuple args ->
                  let len = List.length args in
                  let () = Hashtbl.add scope' 0 fn in
                  List.iteri (fun i arg -> Hashtbl.add scope' (i + 1) arg) args;
                  Array.iteri
                    (fun i v -> Hashtbl.add scope' (len + i + 1) v)
                    closure
              | _ ->
                  let () = Hashtbl.add scope' 0 fn in
                  Hashtbl.add scope' 1 arg;
                  Array.iteri (fun i v -> Hashtbl.add scope' (i + 2) v) closure);
              Stack.push scope cs;
              Stack.push (i + 1) ls;
              helper expr loc scope'
          | _, Values.Builtin body ->
              Stack.push (body arg) vs;
              helper expr (i + 1) scope
          | _ -> assert false)
      | Ir3.FnTailCall -> (
          let arg = Stack.pop vs in
          let fn = Stack.pop vs in
          match (arg, fn) with
          | _, Values.Fn (closure, loc) ->
              let scope' = Hashtbl.create 0 in
              (match arg with
              | Values.Tuple args ->
                  let len = List.length args in
                  let () = Hashtbl.add scope' 0 fn in
                  List.iteri (fun i arg -> Hashtbl.add scope' (i + 1) arg) args;
                  Array.iteri
                    (fun i v -> Hashtbl.add scope' (len + i + 1) v)
                    closure
              | _ ->
                  let () = Hashtbl.add scope' 0 fn in
                  Hashtbl.add scope' 1 arg;
                  Array.iteri (fun i v -> Hashtbl.add scope' (i + 2) v) closure);
              helper expr loc scope'
          | _, Values.Builtin body ->
              Stack.push (body arg) vs;
              helper expr (i + 1) scope
          | _ -> assert false)
      (* decs *)
      | Ir3.Bind id ->
          let v = Stack.pop vs in
          Hashtbl.add scope id v;
          Stack.push Values.Void vs;
          helper expr (i + 1) scope
      (* skip if true *)
      | Ir3.If -> (
          match Stack.pop vs with
          | Values.Bool true -> helper expr (i + 2) scope
          | Values.Bool false -> helper expr (i + 1) scope
          | _ -> assert false)
      | Ir3.Jmp n -> helper expr (i + n) scope
      | Ir3.JmpBck -> helper expr (Stack.pop ls) (Stack.pop cs)
      (* stack *)
      | Ir3.Pop ->
          let _ = Stack.pop vs in
          helper expr (i + 1) scope
  in
  helper es 0 scope

let run ds =
  let scope = Hashtbl.create 0 in
  List.iteri
    (fun i (bfn : Builtins.builtinFn) ->
      Hashtbl.add scope i (Values.Builtin bfn.def))
    Builtins.builtins;
  exec ds scope;
  flush stdout
