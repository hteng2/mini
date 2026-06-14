exception Div of Loc.range
exception Range of Loc.range
exception Break of Loc.range
exception Continue of Loc.range
exception Return of Values.value * Values.value Scopes.t list
exception NoReturn of Loc.range

let rec eval_expr (expr : Ast.expr) (scopes : Values.value Scopes.t list) :
    Values.value * Values.value Scopes.t list =
  match expr.v with
  | Ast.Num n -> (Values.Int n, scopes)
  | Ast.Name name -> (Scopes.search_scopes scopes name, scopes)
  | Ast.True -> (Values.Bool true, scopes)
  | Ast.False -> (Values.Bool false, scopes)
  | Ast.Void -> (Values.Void, scopes)
  | Ast.Neg expr' -> (
      match eval_expr expr' scopes with
      | Values.Int n, scopes' -> (Values.Int (-n), scopes')
      | _ -> assert false)
  | Ast.Pos expr' -> (
      match eval_expr expr' scopes with
      | Values.Int n, scopes' -> (Values.Int n, scopes')
      | _ -> assert false)
  | Ast.Not expr' -> (
      match eval_expr expr' scopes with
      | Values.Bool b, scopes' -> (Values.Bool (not b), scopes')
      | _ -> assert false)
  | Ast.Eq (e1, e2) -> (
      let v1, scopes' = eval_expr e1 scopes in
      let v2, scopes'' = eval_expr e2 scopes' in
      match (v1, v2) with
      | Values.Bool v1, Values.Bool v2 -> (Values.Bool (v1 = v2), scopes'')
      | Values.Int v1, Values.Int v2 -> (Values.Bool (v1 = v2), scopes'')
      | _ -> assert false)
  | Ast.Gt (e1, e2) -> (
      let v1, scopes' = eval_expr e1 scopes in
      let v2, scopes'' = eval_expr e2 scopes' in
      match (v1, v2) with
      | Values.Int v1, Values.Int v2 -> (Values.Bool (v1 > v2), scopes'')
      | _ -> assert false)
  | Ast.Lt (e1, e2) -> (
      let v1, scopes' = eval_expr e1 scopes in
      let v2, scopes'' = eval_expr e2 scopes' in
      match (v1, v2) with
      | Values.Int v1, Values.Int v2 -> (Values.Bool (v1 < v2), scopes'')
      | _ -> assert false)
  | Ast.Add (e1, e2) -> (
      let v1, scopes' = eval_expr e1 scopes in
      let v2, scopes'' = eval_expr e2 scopes' in
      match (v1, v2) with
      | Values.Int v1, Values.Int v2 -> (Values.Int (v1 + v2), scopes'')
      | _ -> assert false)
  | Ast.Sub (e1, e2) -> (
      let v1, scopes' = eval_expr e1 scopes in
      let v2, scopes'' = eval_expr e2 scopes' in
      match (v1, v2) with
      | Values.Int v1, Values.Int v2 -> (Values.Int (v1 - v2), scopes'')
      | _ -> assert false)
  | Ast.Mul (e1, e2) -> (
      let v1, scopes' = eval_expr e1 scopes in
      let v2, scopes'' = eval_expr e2 scopes' in
      match (v1, v2) with
      | Values.Int v1, Values.Int v2 -> (Values.Int (v1 * v2), scopes'')
      | _ -> assert false)
  | Ast.Div (e1, e2) -> (
      let v1, scopes' = eval_expr e1 scopes in
      let v2, scopes'' = eval_expr e2 scopes' in
      match (v1, v2) with
      | Values.Int v1, Values.Int 0 -> raise (Div expr.span)
      | Values.Int v1, Values.Int v2 -> (Values.Int (v1 / v2), scopes'')
      | _ -> assert false)
  | Ast.Mod (e1, e2) -> (
      let v1, scopes' = eval_expr e1 scopes in
      let v2, scopes'' = eval_expr e2 scopes' in
      match (v1, v2) with
      | Values.Int v1, Values.Int 0 -> raise (Div expr.span)
      | Values.Int v1, Values.Int v2 -> (Values.Int (v1 mod v2), scopes'')
      | _ -> assert false)
  | Ast.And (e1, e2) -> (
      let v1, scopes' = eval_expr e1 scopes in
      let v2, scopes'' = eval_expr e2 scopes' in
      match (v1, v2) with
      | Values.Bool v1, Values.Bool v2 -> (Values.Bool (v1 && v2), scopes'')
      | _ -> assert false)
  | Ast.Or (e1, e2) -> (
      let v1, scopes' = eval_expr e1 scopes in
      let v2, scopes'' = eval_expr e2 scopes' in
      match (v1, v2) with
      | Values.Bool v1, Values.Bool v2 -> (Values.Bool (v1 || v2), scopes'')
      | _ -> assert false)
  | Ast.Xor (e1, e2) -> (
      let v1, scopes' = eval_expr e1 scopes in
      let v2, scopes'' = eval_expr e2 scopes' in
      match (v1, v2) with
      | Values.Bool v1, Values.Bool v2 -> (Values.Bool (v1 <> v2), scopes'')
      | _ -> assert false)
  | Ast.List es ->
      let vs, scopes' = eval_exprs es scopes [] in
      (Values.List (Array.of_list vs), scopes')
  | Ast.At (e1, e2) -> (
      let v1, scopes' = eval_expr e1 scopes in
      let v2, scopes'' = eval_expr e2 scopes' in
      match (v1, v2) with
      | Values.List l, Values.Int i ->
          if 0 <= i && i < Array.length l then (Array.get l i, scopes'')
          else raise (Range expr.span)
      | _ -> assert false)
  | Ast.FnVal (ps, t, body) -> (Values.Fn (ps, body), scopes)
  | Ast.FnCall (fn, args) -> (
      let fn', scopes' = eval_expr fn scopes in
      let args', scopes'' = eval_exprs args scopes' [] in
      match fn' with
      | Values.Fn (ps, body) -> (
          let scopes''' =
            List.fold_left2
              (fun acc ->
                fun ({ v = name, _ } : Ast.param) ->
                 fun arg -> Scopes.add_to_scope acc name arg)
              (Scopes.add_scope scopes'')
              ps args'
          in
          try
            let _ = eval_dec body scopes''' in
            raise (NoReturn expr.span)
          with Return (a, b) -> (a, b))
      | _ -> assert false)

and eval_exprs es scopes acc =
  match es with
  | [] -> (acc, scopes)
  | e :: es' ->
      let v, scopes' = eval_expr e scopes in
      eval_exprs es' scopes' (v :: acc)

and eval_id (id : Ast.identifier) (scopes : Values.value Scopes.t list) :
    Values.value * Values.value Scopes.t list =
  match id.v with
  | Ast.IdName name -> (Scopes.search_scopes scopes name, scopes)
  | Ast.IdAt (id', e) -> (
      let id'', scopes' = eval_id id' scopes in
      let v, scopes'' = eval_expr e scopes' in
      match (id'', v) with
      | Values.List l, Values.Int n -> (l.(n), scopes'')
      | _ -> assert false)

and eval_varset (id : Ast.identifier) (e : Ast.expr)
    (scopes : Values.value Scopes.t list) : Values.value Scopes.t list =
  match id.v with
  | Ast.IdName name ->
      let v, scopes' = eval_expr e scopes in
      Scopes.update_scopes scopes' name v
  | Ast.IdAt (id', n) -> (
      let id'', scopes' = eval_id id' scopes in
      let i, scopes'' = eval_expr n scopes' in
      let v, scopes''' = eval_expr e scopes'' in
      match (id'', i) with
      | Values.List l, Values.Int i ->
          if 0 <= i && i < Array.length l then (
            l.(i) <- v;
            scopes''')
          else raise (Range id.span)
      | _ -> assert false)

and eval_dec (d : Ast.dec) (scopes : Values.value Scopes.t list) :
    Values.value Scopes.t list =
  match d.v with
  | Ast.Let (name, expr) ->
      let v, scopes' = eval_expr expr scopes in
      Scopes.add_to_scope scopes' name v
  | Ast.Var (name, expr) ->
      let v, scopes' = eval_expr expr scopes in
      Scopes.add_to_scope scopes' name v
  | Ast.VarSet (id, expr) -> eval_varset id expr scopes
  | Ast.Print expr ->
      let v, scopes' = eval_expr expr scopes in
      print_val v;
      scopes'
  | Ast.Println expr ->
      let v, scopes' = eval_expr expr scopes in
      print_val v;
      print_newline ();
      scopes'
  | Ast.If (expr, body1, body2) -> (
      let v, scopes' = eval_expr expr scopes in
      match (v, body2) with
      | Values.Bool true, _ ->
          let scopes'' = Scopes.add_scope scopes' in
          let scopes''' = eval_dec body1 scopes'' in
          Scopes.pop_scope scopes'''
      | Values.Bool false, Some body2 ->
          let scopes'' = Scopes.add_scope scopes' in
          let scopes''' = eval_dec body2 scopes'' in
          Scopes.pop_scope scopes'''
      | _ -> scopes')
  | Ast.While (expr, body) ->
      let rec run scopes =
        let v, scopes' = eval_expr expr scopes in
        match v with
        | Values.Bool true ->
            let scopes'' = Scopes.add_scope scopes' in
            let scopes''' = eval_dec body scopes'' in
            run (Scopes.pop_scope scopes''')
        | _ -> scopes'
      in
      run scopes
  | Ast.Break -> raise (Break d.span)
  | Ast.Continue -> raise (Continue d.span)
  | Ast.Return e ->
      let v, scopes' = eval_expr e scopes in
      raise (Return (v, scopes'))
  | Ast.Block p -> (
      match eval p (Scopes.add_scope scopes) with
      | [] -> assert false
      | _ :: scopes' -> scopes')

and print_val (v : Values.value) =
  match v with
  | Values.Bool v -> Printf.printf "%s : bool" (if v then "true" else "false")
  | Values.Int v -> Printf.printf "%d : int" v
  | Values.Void -> Printf.printf "void : void"
  | Values.List arr ->
      Printf.printf "[";
      print_arr (Array.to_list arr);
      Printf.printf "] : list"
  | Values.Fn (ps, body) -> Printf.printf " - : function"

and print_arr (arr : Values.value list) =
  match arr with
  | v :: [] -> print_val v
  | v :: arr' ->
      print_val v;
      print_string ", ";
      print_arr arr'
  | [] -> assert false

and eval (ds : Ast.dec list) (scopes : Values.value Scopes.t list) :
    Values.value Scopes.t list =
  match ds with
  | [] -> scopes
  | d :: ds' ->
      let scope' = eval_dec d scopes in
      eval ds' scope'
