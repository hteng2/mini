exception Div
exception Range

let rec eval_expr (expr : Ast.expr) (scopes : Values.value Scopes.t list) :
    Values.value =
  match expr.v with
  | Ast.Num n -> Values.Int n
  | Ast.Id id -> eval_id id scopes
  | Ast.True -> Values.Bool true
  | Ast.False -> Values.Bool false
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
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Bool v1, Values.Bool v2 -> Values.Bool (v1 = v2)
      | Values.Int v1, Values.Int v2 -> Values.Bool (v1 = v2)
      | _ -> assert false)
  | Ast.Gt (e1, e2) -> (
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Int v1, Values.Int v2 -> Values.Bool (v1 > v2)
      | _ -> assert false)
  | Ast.Lt (e1, e2) -> (
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Int v1, Values.Int v2 -> Values.Bool (v1 < v2)
      | _ -> assert false)
  | Ast.Add (e1, e2) -> (
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 + v2)
      | _ -> assert false)
  | Ast.Sub (e1, e2) -> (
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 - v2)
      | _ -> assert false)
  | Ast.Mul (e1, e2) -> (
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 * v2)
      | _ -> assert false)
  | Ast.Div (e1, e2) -> (
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Int v1, Values.Int 0 -> raise Div
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 / v2)
      | _ -> assert false)
  | Ast.Mod (e1, e2) -> (
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Int v1, Values.Int 0 -> raise Div
      | Values.Int v1, Values.Int v2 -> Values.Int (v1 mod v2)
      | _ -> assert false)
  | Ast.And (e1, e2) -> (
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Bool v1, Values.Bool v2 -> Values.Bool (v1 && v2)
      | _ -> assert false)
  | Ast.Or (e1, e2) -> (
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Bool v1, Values.Bool v2 -> Values.Bool (v1 || v2)
      | _ -> assert false)
  | Ast.Xor (e1, e2) -> (
      match (eval_expr e1 scopes, eval_expr e2 scopes) with
      | Values.Bool v1, Values.Bool v2 -> Values.Bool (v1 <> v2)
      | _ -> assert false)
  | Ast.List es -> Values.List (Array.of_list (eval_list es scopes))

and eval_list es scopes =
  match es with
  | [] -> []
  | e :: es' -> eval_expr e scopes :: eval_list es' scopes

and eval_id (id : Ast.v) (scopes : Values.value Scopes.t list) : Values.value =
  match id with
  | Ast.Name name -> Scopes.search_scopes scopes name
  | Ast.At (id', e) -> (
      match (eval_id id' scopes, eval_expr e scopes) with
      | Values.List l, Values.Int n -> Array.get l n
      | _ -> assert false)

let rec eval_varset (id : Ast.v) (v : Values.value)
    (scopes : Values.value Scopes.t list) : Values.value Scopes.t list =
  match id with
  | Ast.Name name -> Scopes.update_scopes scopes name v
  | Ast.At (id', e) -> (
      match eval_expr e scopes with
      | Values.Int i -> (
          match eval_id id' scopes with
          | Values.List l ->
              if 0 <= i && i < Array.length l then (
                Array.set l i v;
                scopes)
              else raise Range
          | _ -> assert false)
      | _ -> assert false)

let rec eval_dec (d : Ast.dec) (scopes : Values.value Scopes.t list) :
    Values.value Scopes.t list =
  match d.v with
  | Ast.Let (name, expr) ->
      Scopes.add_to_scope scopes name (eval_expr expr scopes)
  | Ast.Var (name, expr) ->
      Scopes.add_to_scope scopes name (eval_expr expr scopes)
  | Ast.VarSet (id, expr) ->
      let v = eval_expr expr scopes in
      eval_varset id v scopes
  | Ast.Print expr ->
      print_val (eval_expr expr scopes);
      scopes
  | Ast.Println expr ->
      print_val (eval_expr expr scopes);
      print_newline ();
      scopes
  | Ast.If (Ast.IfThen (expr, body)) -> (
      match eval_expr expr scopes with
      | Values.Bool true -> (
          match eval body (Scopes.add_scope scopes) with
          | [] -> assert false
          | _ :: scopes' -> scopes')
      | _ -> scopes)
  | Ast.If (Ast.IfThenElse (expr, body, body2)) -> (
      match eval_expr expr scopes with
      | Values.Bool true -> (
          match eval body (Scopes.add_scope scopes) with
          | [] -> assert false
          | _ :: scopes' -> scopes')
      | Values.Bool false -> (
          match eval body2 (Scopes.add_scope scopes) with
          | [] -> assert false
          | _ :: scopes' -> scopes')
      | _ -> assert false)
  | Ast.While (expr, body) ->
      let rec run scopes =
        match eval_expr expr scopes with
        | Values.Bool true -> (
            match eval body (Scopes.add_scope scopes) with
            | [] -> assert false
            | _ :: scopes' -> run scopes')
        | _ -> scopes
      in
      run scopes

and print_val (v : Values.value) =
  match v with
  | Values.Bool v -> Printf.printf "%s : bool" (if v then "true" else "false")
  | Values.Int v -> Printf.printf "%d : int" v
  | Values.List arr ->
      Printf.printf "[";
      print_arr (Array.to_list arr);
      Printf.printf "] : list"

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
