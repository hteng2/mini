exception Div of Loc.range
exception Range of Loc.range
exception Break
exception Continue
exception Return of Values.value
exception NoReturn of Loc.range

let run_with_scope (scopes : Values.value Scopes.t list)
    (f : Values.value Scopes.t list -> 'a) : 'a =
  f (Scopes.add_scope scopes)

let rec eval_expr (expr : Ast.expr) (scopes : Values.value Scopes.t list) :
    Values.value =
  match expr.v with
  | Ast.Num n -> Values.Int n
  | Ast.Name name -> Scopes.search_scopes scopes name
  | Ast.True -> Values.Bool true
  | Ast.False -> Values.Bool false
  | Ast.Void -> Values.Void
  | Ast.Neg expr' -> (
      match eval_expr expr' scopes with
      | Values.Int n -> Values.Int (-n)
      | _ -> assert false)
  | Ast.Pos expr' -> (
      match eval_expr expr' scopes with
      | Values.Int n -> Values.Int n
      | _ -> assert false)
  | Ast.Not expr' -> (
      match eval_expr expr' scopes with
      | Values.Bool b -> Values.Bool (not b)
      | _ -> assert false)
  | Ast.Eq (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Bool v1, Values.Bool v2 -> Values.Bool (v1 = v2)
      | Values.Int v1, Values.Int v2 -> Values.Bool (v1 = v2)
      | _ -> assert false)
  | Ast.Gt (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Int v1, Values.Int v2 -> Values.Bool (v1 > v2)
      | _ -> assert false)
  | Ast.Lt (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Int v1, Values.Int v2 -> Values.Bool (v1 < v2)
      | _ -> assert false)
  | Ast.Add (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 + v2)
      | _ -> assert false)
  | Ast.Sub (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 - v2)
      | _ -> assert false)
  | Ast.Mul (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 * v2)
      | _ -> assert false)
  | Ast.Div (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Int v1, Values.Int 0 -> raise (Div expr.span)
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 / v2)
      | _ -> assert false)
  | Ast.Mod (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Int v1, Values.Int 0 -> raise (Div expr.span)
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 mod v2)
      | _ -> assert false)
  | Ast.And (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Bool v1, Values.Bool v2 -> Values.Bool (v1 && v2)
      | _ -> assert false)
  | Ast.Or (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Bool v1, Values.Bool v2 -> Values.Bool (v1 || v2)
      | _ -> assert false)
  | Ast.Xor (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.Bool v1, Values.Bool v2 -> Values.Bool (v1 <> v2)
      | _ -> assert false)
  | Ast.List es ->
      let vs = eval_exprs es scopes [] in
      Values.List (Array.of_list vs)
  | Ast.At (e1, e2) -> (
      let v1 = eval_expr e1 scopes in
      let v2 = eval_expr e2 scopes in
      match (v1, v2) with
      | Values.List l, Values.Int i ->
          if 0 <= i && i < Array.length l then Array.get l i
          else raise (Range expr.span)
      | _ -> assert false)
  | Ast.FnVal (ps, t, body) -> Values.Fn (ps, body, scopes)
  | Ast.FnCall (fn, args) -> (
      let fn' = eval_expr fn scopes in
      let args' = eval_exprs args scopes [] in
      match fn' with
      | Values.Fn (ps, body, closure) ->
          run_with_scope closure (fun closure' ->
              List.iter2
                (fun ({ v = name, _ } : Ast.param) ->
                  fun arg -> Scopes.add_to_scope closure' name arg)
                ps args';
              try
                eval_dec body closure';
                raise (NoReturn expr.span)
              with Return v -> v)
      | _ -> assert false)

and eval_exprs es scopes acc =
  match es with
  | [] -> acc
  | e :: es' ->
      let v = eval_expr e scopes in
      eval_exprs es' scopes (v :: acc)

and eval_id (id : Ast.identifier) (scopes : Values.value Scopes.t list) :
    Values.value =
  match id.v with
  | Ast.IdName name -> Scopes.search_scopes scopes name
  | Ast.IdAt (id', e) -> (
      let id'' = eval_id id' scopes in
      let v = eval_expr e scopes in
      match (id'', v) with
      | Values.List l, Values.Int n -> l.(n)
      | _ -> assert false)

and eval_varset (id : Ast.identifier) (e : Ast.expr)
    (scopes : Values.value Scopes.t list) : unit =
  match id.v with
  | Ast.IdName name ->
      let v = eval_expr e scopes in
      Scopes.update_scopes scopes name v
  | Ast.IdAt (id', n) -> (
      let id'' = eval_id id' scopes in
      let i = eval_expr n scopes in
      let v = eval_expr e scopes in
      match (id'', i) with
      | Values.List l, Values.Int i ->
          if 0 <= i && i < Array.length l then l.(i) <- v
          else raise (Range id.span)
      | _ -> assert false)

and eval_dec (d : Ast.dec) (scopes : Values.value Scopes.t list) : unit =
  match d.v with
  | Ast.Let (name, expr) ->
      let v = eval_expr expr scopes in
      Scopes.add_to_scope scopes name v
  | Ast.Var (name, expr) ->
      let v = eval_expr expr scopes in
      Scopes.add_to_scope scopes name v
  | Ast.VarSet (id, expr) -> eval_varset id expr scopes
  | Ast.Print expr ->
      let v = eval_expr expr scopes in
      print_val v
  | Ast.Println expr ->
      let v = eval_expr expr scopes in
      print_val v;
      print_newline ()
  | Ast.If (expr, body1, body2) -> (
      let v = eval_expr expr scopes in
      match (v, body2) with
      | Values.Bool true, _ ->
          run_with_scope scopes (fun scopes'' -> eval_dec body1 scopes'')
      | Values.Bool false, Some body2 ->
          run_with_scope scopes (fun scopes'' -> eval_dec body2 scopes'')
      | _ -> ())
  | Ast.While (expr, body) ->
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
  | Ast.Break -> raise Break
  | Ast.Continue -> raise Continue
  | Ast.Return e ->
      let v = eval_expr e scopes in
      raise (Return v)
  | Ast.Block p -> run_with_scope scopes (fun scopes' -> eval p scopes')

and print_val (v : Values.value) =
  match v with
  | Values.Bool v -> Printf.printf "%s : bool" (if v then "true" else "false")
  | Values.Int v -> Printf.printf "%d : int" v
  | Values.Void -> Printf.printf "void : void"
  | Values.List arr ->
      Printf.printf "[";
      print_arr (Array.to_list arr);
      Printf.printf "] : list"
  | Values.Fn (ps, body, closure) -> Printf.printf " - : function"

and print_arr (arr : Values.value list) =
  match arr with
  | v :: [] -> print_val v
  | v :: arr' ->
      print_val v;
      print_string ", ";
      print_arr arr'
  | [] -> assert false

and eval (ds : Ast.dec list) (scopes : Values.value Scopes.t list) : unit =
  List.iter (fun d -> eval_dec d scopes) ds
