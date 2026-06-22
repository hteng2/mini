exception Div
exception Range
exception Break
exception Continue
exception Return of Values.v
exception NoReturn

let run_with_scope (scopes : Values.value Closure.t list)
    (f : Values.value Closure.t list -> 'a) : 'a =
  f (Closure.empty () :: scopes)

let rec eval_expr (expr : Ir.expr) (scopes : Values.value Closure.t list) :
    Values.v =
  match expr with
  | Ir.Num n -> Values.Int n
  | Ir.Name name -> Values.value_to_v (Option.get (Closure.search scopes name))
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
      let vs = eval_exprs es scopes [] in
      Values.List (Array.of_list (List.map ref vs))
  | Ir.At (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.List l, Values.Int i ->
          if 0 <= i && i < Array.length l then !(Array.get l i) else raise Range
      | _ -> assert false)
  | Ir.FnVal (ps, c, body) ->
      let c' = Closure.empty () in
      Closure.iter
        (fun name _ ->
          Closure.set c' name (Option.get (Closure.search scopes name)))
        c;
      Values.Fn (ps, c', body)
  | Ir.FnCall (fn, args) -> (
      let fn' = eval_expr fn scopes in
      let args' = eval_exprs args scopes [] in
      match fn' with
      | Values.Fn (ps, closure, body) -> (
          List.iter2
            (fun name ->
              fun arg ->
               if name = "_" then ()
               else Closure.set closure name (Values.Var (ref arg)))
            ps args';
          try
            eval_dec body [ closure ];
            raise NoReturn
          with Return v -> v)
      | _ -> assert false)

and eval_exprs es scopes acc : Values.v list =
  match es with
  | [] -> acc
  | e :: es' ->
      let v = eval_expr e scopes in
      eval_exprs es' scopes (v :: acc)

and id_to_ref (id : Ir.identifier) (scopes : Values.value Closure.t list) :
    Values.v ref =
  match id with
  | Ir.IdName name -> (
      match Closure.search scopes name with
      | Some (Values.Var v) -> v
      | _ -> assert false)
  | Ir.IdAt (id', e) -> (
      let r = id_to_ref id' scopes in
      let v = eval_expr e scopes in
      match (r, v) with
      | { contents = Values.List l }, Values.Int n -> l.(n)
      | _ -> assert false)

and eval_varset (id : Ir.identifier) (e : Ir.expr)
    (scopes : Values.value Closure.t list) : unit =
  let r = id_to_ref id scopes in
  let v = eval_expr e scopes in
  r := v

and eval_dec (d : Ir.dec) (scopes : Values.value Closure.t list) : unit =
  match d with
  | Ir.Let (name, expr) ->
      let v = eval_expr expr scopes in
      if name = "_" then ()
      else Closure.set (List.nth scopes 0) name (Values.Const v)
  | Ir.Var (name, expr) ->
      let v = eval_expr expr scopes in
      if name = "_" then ()
      else Closure.set (List.nth scopes 0) name (Values.Var (ref v))
  | Ir.VarSet (id, expr) -> eval_varset id expr scopes
  | Ir.Print expr ->
      let v = eval_expr expr scopes in
      print_val v
  | Ir.Println expr ->
      let v = eval_expr expr scopes in
      print_val v;
      print_newline ()
  | Ir.If (expr, body1, body2) -> (
      let v = eval_expr expr scopes in
      match (v, body2) with
      | Values.Bool true, _ ->
          run_with_scope scopes (fun scopes'' -> eval_dec body1 scopes'')
      | Values.Bool false, Some body2 ->
          run_with_scope scopes (fun scopes'' -> eval_dec body2 scopes'')
      | _ -> ())
  | Ir.While (expr, body) ->
      let rec run () =
        let v = eval_expr expr scopes in
        match v with
        | Values.Bool true -> (
            try
              run_with_scope scopes (fun scopes'' -> eval_dec body scopes'');
              run ()
            with
            | Break -> ()
            | Continue -> run ())
        | _ -> ()
      in
      run ()
  | Ir.Break -> raise Break
  | Ir.Continue -> raise Continue
  | Ir.Return e ->
      let v = eval_expr e scopes in
      raise (Return v)
  | Ir.Block p -> run_with_scope scopes (fun scopes' -> eval p scopes')

and print_val (v : Values.v) =
  match v with
  | Values.Bool v -> Printf.printf "%s : bool" (if v then "true" else "false")
  | Values.Int v -> Printf.printf "%d : int" v
  | Values.Void -> Printf.printf "void : void"
  | Values.List arr ->
      Printf.printf "[";
      print_arr (List.map ( ! ) (Array.to_list arr));
      Printf.printf "] : list"
  | Values.Fn (ps, body, closure) -> Printf.printf " - : function"

and print_arr (arr : Values.v list) =
  match arr with
  | v :: [] -> print_val v
  | v :: arr' ->
      print_val v;
      print_string ", ";
      print_arr arr'
  | [] -> assert false

and eval (ds : Ir.dec list) (scopes : Values.value Closure.t list) : unit =
  List.iter (fun d -> eval_dec d scopes) ds
