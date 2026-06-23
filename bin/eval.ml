exception Div
exception Range
exception Break
exception Continue
exception Return of Values.v
exception NoReturn

let run_with_scope scopes f = f (Closure.empty () :: scopes)

let rec eval_expr expr scopes =
  match expr with
  | Ir.Num n -> Values.Int n
  | Ir.Char c -> Values.Char c
  | Ir.Str s -> Values.Str s
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

and eval_exprs es scopes acc =
  match es with
  | [] -> acc
  | e :: es' ->
      let v = eval_expr e scopes in
      eval_exprs es' scopes (v :: acc)

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
  | Ir.IdName name -> (
      let v = eval_expr e scopes in
      let r = Closure.search scopes name in
      match r with Some (Values.Var r') -> r' := v | _ -> assert false)
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

and eval_dec d scopes =
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
  | Ir.Print expr -> (
      let v = eval_expr expr scopes in
      match v with Values.Str s -> print_string s | _ -> assert false)
  | Ir.Println expr -> (
      let v = eval_expr expr scopes in
      match v with Values.Str s -> print_endline s | _ -> assert false)
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

and eval ds scopes =
  let rec f ds =
    match ds () with
    | Stream.End -> ()
    | Stream.Head (d, ds') ->
        eval_dec d scopes;
        f ds'
  in
  f ds

let run ds = eval ds [ Closure.empty () ]
