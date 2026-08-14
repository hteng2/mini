(* typechecker (ir1 -> ir2)
    type of exprs*)

open Minic_lib

exception TypeError of Errors.error

(* quickly check for type matching *)
let force_type t1 t2 e = if t1 = t2 then () else raise (TypeError e)

(* convert param parsed types to actual types *)
let rec translate_type (t : Ir1.resolved_type) (tvs : int list) =
  match t.v with
  | Ir1.RtBase "int" -> Types.Int
  | Ir1.RtBase "float" -> Types.Float
  | Ir1.RtBase "bool" -> Types.Bool
  | Ir1.RtBase "char" -> Types.Char
  | Ir1.RtBase "void" -> Types.Void
  | Ir1.RtBase s ->
      raise
        (TypeError
           { v = Printf.sprintf "unrecognized type %s" s; span = t.span })
  | Ir1.RtVar n ->
      if List.for_all (fun tv -> tv != n) tvs then Types.Some n else Types.All n
  | Ir1.RtList t' -> Types.List (translate_type t' tvs)
  | Ir1.RtFn (t1, t2) -> Types.Fn (translate_type t1 tvs, translate_type t2 tvs)
  | Ir1.RtTup ts -> (
      let ts' = List.map (fun t -> translate_type t tvs) ts in
      match List.length ts' with
      | 0 -> Types.Void
      | 1 -> List.hd ts'
      | _ -> Types.Tuple ts')

(* check param type, uses universal quantifiers for returned type, but assigns symbols to concrete type *)
let rec specify t =
  match t with
  | Types.All n -> Types.Some n
  | Types.List t -> Types.List (specify t)
  | Types.Fn (f, t) -> Types.Fn (specify f, specify t)
  | Types.Tuple ts -> Types.Tuple (List.map specify ts)
  | t -> t

let rec check_param prm symbols tvs =
  match prm with
  | Ir1.PrmUnit -> (Types.Void, Ir2.PtrnUnit)
  | Ir1.PrmLeaf (i, t) ->
      let t' = translate_type t tvs in
      let t'' = specify t' in
      Hashtbl.add symbols i t'';
      (t', Ir2.PtrnLeaf i)
  | Ir1.PrmTuple ps ->
      let ts, ps' =
        ps |> List.map (fun p -> check_param p symbols tvs) |> List.split
      in
      (Types.Tuple ts, Ir2.PtrnTuple ps')

(* HM unification *)
let rec walk t assignments : Types.t =
  match t with
  | Types.All n -> (
      match Hashtbl.find_opt assignments n with
      | None -> t
      | Some t' -> walk t' assignments)
  | _ -> t

let unify t0 t1 assignments span =
  let rec occurs n t =
    match walk t assignments with
    | Types.All n' -> n == n'
    | Types.Some _ | Types.Int | Types.Float | Types.Bool | Types.Char
    | Types.Void ->
        false
    | Types.List t' -> occurs n t'
    | Types.Fn (f, t) -> occurs n f || occurs n t
    | Types.Tuple ts -> List.fold_left (fun acc t -> acc || occurs n t) false ts
  in
  let assign n t =
    let t' = walk t assignments in
    if occurs n t then
      raise
        (TypeError
           {
             v =
               Printf.sprintf "failed to unify: %s occurs in %s (from %s)"
                 (Types.t_to_str (Types.All n))
                 (Types.t_to_str t') (Types.t_to_str t);
             span;
           })
    else Hashtbl.replace assignments n t'
  in
  let rec helper t0 t1 =
    let t0', t1' = (walk t0 assignments, walk t1 assignments) in
    match (t0', t1') with
    | Types.All n0, Types.All n1 when n0 = n1 -> t0'
    | Types.All n0, _ ->
        assign n0 t1';
        t1'
    | _, Types.All n1 ->
        assign n1 t0';
        t0'
    | Types.Some n0, Types.Some n1 ->
        if n0 = n1 then t0'
        else
          raise
            (TypeError
               {
                 v =
                   Printf.sprintf "failed to unify %s with %s"
                     (Types.t_to_str t0) (Types.t_to_str t1);
                 span;
               })
    | Types.Int, Types.Int
    | Types.Float, Types.Float
    | Types.Bool, Types.Bool
    | Types.Char, Types.Char
    | Types.Void, Types.Void ->
        t0
    | Types.List inner0, Types.List inner1 -> Types.List (helper inner0 inner1)
    | Types.Fn (from0, to0), Types.Fn (from1, to1) ->
        Types.Fn (helper from0 from1, helper to0 to1)
    | Types.Tuple ts0, Types.Tuple ts1 ->
        Types.Tuple (List.map2 (fun t0 t1 -> helper t0 t1) ts0 ts1)
    | _ ->
        raise
          (TypeError
             {
               v =
                 Printf.sprintf "failed to unify %s with %s" (Types.t_to_str t0)
                   (Types.t_to_str t1);
               span;
             })
  in
  helper t0 t1

let rec match_ptrn_type ptrn span t symbols : Ir2.pattern =
  match (ptrn, t) with
  | Ir1.PtrnUnit, Types.Void -> Ir2.PtrnUnit
  | Ir1.PtrnLeaf i, t ->
      Hashtbl.add symbols i t;
      Ir2.PtrnLeaf i
  | Ir1.PtrnTuple ps, Types.Tuple ts when List.length ps = List.length ts ->
      let ps' = List.map2 (fun p t -> match_ptrn_type p span t symbols) ps ts in
      Ir2.PtrnTuple ps'
  | _ -> raise (TypeError { v = "could not match pattern to type"; span })

(* apply type induction rules and convert to ops *)
let rec check_expr ({ v; span } : Ir1.expr) symbols chainhead =
  let (ir' : Ir2.expr), t =
    match v with
    (* atoms *)
    | Ir1.Int n -> ({ v = Ir2.Int n; span }, Types.Int)
    | Ir1.Float n -> ({ v = Ir2.Float n; span }, Types.Float)
    | Ir1.Char c -> ({ v = Ir2.Char c; span }, Types.Char)
    | Ir1.Str s ->
        ( {
            v =
              Ir2.List
                (Array.init (String.length s) (fun i ->
                     ({ v = Ir2.Char s.[i]; span } : Ir2.expr)));
            span;
          },
          Types.List Types.Char )
    | Ir1.Name name -> ({ v = Ir2.Name name; span }, Hashtbl.find symbols name)
    | Ir1.Bool b -> ({ v = Ir2.Bool b; span }, Types.Bool)
    | Ir1.Void -> ({ v = Ir2.Void; span }, Types.Void)
    (* unops *)
    | Ir1.Neg e -> (
        let e', t = check_expr e symbols false in
        match t with
        | Types.Int -> ({ v = Ir2.INeg e'; span }, t)
        | Types.Float -> ({ v = Ir2.FNeg e'; span }, t)
        | _ ->
            raise
              (TypeError
                 {
                   v =
                     Format.sprintf "expected int or float, got %s"
                       (Types.t_to_str t);
                   span = e.span;
                 }))
    | Ir1.Pos e -> (
        let e', t = check_expr e symbols false in
        match t with
        | Types.Int -> (e', t)
        | Types.Float -> (e', t)
        | _ ->
            raise
              (TypeError
                 {
                   v =
                     Format.sprintf "expected int or float, got %s"
                       (Types.t_to_str t);
                   span = e.span;
                 }))
    | Ir1.Not e ->
        let e', t = check_expr e symbols false in
        force_type t Types.Bool
          {
            v = Format.sprintf "expected boolean, got %s" (Types.t_to_str t);
            span = e.span;
          };
        ({ v = Ir2.Not e'; span }, t)
    (* biops *)
    | Ir1.Eq (e1, e2) -> (
        let e1', t1 = check_expr e1 symbols false in
        let e2', t2 = check_expr e2 symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.IEq (e1', e2'); span }, Types.Bool)
        | Types.Bool, Types.Bool ->
            ({ v = Ir2.BEq (e1', e2'); span }, Types.Bool)
        | Types.Char, Types.Char ->
            ({ v = Ir2.CEq (e1', e2'); span }, Types.Bool)
        | _ ->
            raise
              (TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int = int, bool = bool, or char = char, got \
                        %s = %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.Neq (e1, e2) -> (
        let e1', t1 = check_expr e1 symbols false in
        let e2', t2 = check_expr e2 symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.INeq (e1', e2'); span }, Types.Bool)
        | Types.Bool, Types.Bool ->
            ({ v = Ir2.BEq (e1', e2'); span }, Types.Bool)
        | Types.Char, Types.Char ->
            ({ v = Ir2.CNeq (e1', e2'); span }, Types.Bool)
        | _ ->
            raise
              (TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int != int, bool != bool, or char != char, \
                        got %s != %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.Gt (e1, e2) -> (
        let e1', t1 = check_expr e1 symbols false in
        let e2', t2 = check_expr e2 symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.IGt (e1', e2'); span }, Types.Bool)
        | Types.Float, Types.Float ->
            ({ v = Ir2.FGt (e1', e2'); span }, Types.Bool)
        | Types.Char, Types.Char ->
            ({ v = Ir2.CGt (e1', e2'); span }, Types.Bool)
        | _ ->
            raise
              (TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int > int, float > float, or char > char, got \
                        %s > %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.Ge (e1, e2) -> (
        let e1', t1 = check_expr e1 symbols false in
        let e2', t2 = check_expr e2 symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.IGe (e1', e2'); span }, Types.Bool)
        | Types.Char, Types.Char ->
            ({ v = Ir2.CGe (e1', e2'); span }, Types.Bool)
        | _ ->
            raise
              (TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int >= int, float >= float, or char >= char, \
                        got %s >= %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.Lt (e1, e2) -> (
        let e1', t1 = check_expr e1 symbols false in
        let e2', t2 = check_expr e2 symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.ILt (e1', e2'); span }, Types.Bool)
        | Types.Float, Types.Float ->
            ({ v = Ir2.FLt (e1', e2'); span }, Types.Bool)
        | Types.Char, Types.Char ->
            ({ v = Ir2.CLt (e1', e2'); span }, Types.Bool)
        | _ ->
            raise
              (TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int < int, float < float, or char < char, got \
                        %s < %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.Le (e1, e2) -> (
        let e1', t1 = check_expr e1 symbols false in
        let e2', t2 = check_expr e2 symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.ILe (e1', e2'); span }, Types.Bool)
        | Types.Char, Types.Char ->
            ({ v = Ir2.CLe (e1', e2'); span }, Types.Bool)
        | _ ->
            raise
              (TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int < int, float < float, or char < char, got \
                        %s < %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.Add (e1, e2) -> (
        let e1', t1 = check_expr e1 symbols false in
        let e2', t2 = check_expr e2 symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.IAdd (e1', e2'); span }, t1)
        | Types.Float, Types.Float -> ({ v = Ir2.FAdd (e1', e2'); span }, t1)
        | _ ->
            raise
              (TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int + int or float + float, got %s + %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.Sub (e1, e2) -> (
        let e1', t1 = check_expr e1 symbols false in
        let e2', t2 = check_expr e2 symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.ISub (e1', e2'); span }, t1)
        | Types.Float, Types.Float -> ({ v = Ir2.FSub (e1', e2'); span }, t1)
        | _ ->
            raise
              (TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int - int or float - float, got %s - %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.Mul (e1, e2) -> (
        let e1', t1 = check_expr e1 symbols false in
        let e2', t2 = check_expr e2 symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.IMul (e1', e2'); span }, t1)
        | Types.Float, Types.Float -> ({ v = Ir2.FMul (e1', e2'); span }, t1)
        | _ ->
            raise
              (TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int * int or float * float, got %s * %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.Div (e1, e2) -> (
        let e1', t1 = check_expr e1 symbols false in
        let e2', t2 = check_expr e2 symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.IDiv (e1', e2'); span }, t1)
        | Types.Float, Types.Float -> ({ v = Ir2.FDiv (e1', e2'); span }, t1)
        | _ ->
            raise
              (TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int / int or float / float, got %s / %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.Mod (e1, e2) -> (
        let e1', t1 = check_expr e1 symbols false in
        let e2', t2 = check_expr e2 symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.IMod (e1', e2'); span }, t1)
        | _ ->
            raise
              (TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int %% int or float %% float, got %s %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.And (e1, e2) ->
        let e1', t1 = check_expr e1 symbols false in
        let e2', t2 = check_expr e2 symbols false in
        force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
        force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
        ({ v = Ir2.And (e1', e2'); span }, Types.Bool)
    | Ir1.Or (e1, e2) ->
        let e1', t1 = check_expr e1 symbols false in
        let e2', t2 = check_expr e2 symbols false in
        force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
        force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
        ({ v = Ir2.Or (e1', e2'); span }, Types.Bool)
    | Ir1.Xor (e1, e2) ->
        let e1', t1 = check_expr e1 symbols false in
        let e2', t2 = check_expr e2 symbols false in
        force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
        force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
        ({ v = Ir2.Xor (e1', e2'); span }, Types.Bool)
    | Ir1.List es ->
        let rec infer_list es es_acc t =
          match es with
          | [] -> (t, es_acc)
          | e :: es' -> (
              let e', t1 = check_expr e symbols false in
              match t with
              | Some t' when t1 <> t' ->
                  raise
                    (TypeError
                       {
                         v =
                           Format.sprintf
                             "list types do not match, got %s and %s"
                             (Types.t_to_str t1) (Types.t_to_str t');
                         span;
                       })
              | _ -> infer_list es' (e' :: es_acc) (Some t1))
        in
        let t, es' = infer_list es [] None in
        let t' = match t with Some t' -> t' | None -> Void in
        ({ v = Ir2.List (Array.of_list (List.rev es')); span }, Types.List t')
    | Ir1.At (e1, e2) -> (
        let e1', t1 = check_expr e1 symbols false in
        match t1 with
        | Types.List t ->
            let e2', t2 = check_expr e2 symbols false in
            force_type t2 Types.Int { v = "expected integer"; span = e2.span };
            ({ v = Ir2.At (e1', e2'); span }, t)
        | _ ->
            raise
              (TypeError
                 {
                   v = Format.sprintf "list access of %s" (Types.t_to_str t1);
                   span;
                 }))
    | Ir1.Tuple es ->
        let es', ts =
          List.split (List.map (fun e -> check_expr e symbols false) es)
        in
        ({ v = Ir2.Tuple es'; span }, Types.Tuple ts)
    | Ir1.FnVal (tvs, p, closure, rest, self, symcnt, body) ->
        let scope' = Hashtbl.create 0 in
        let id = ref 1 in
        let () =
          List.iter
            (fun name ->
              let t = Hashtbl.find symbols name in
              Hashtbl.add scope' !id t;
              incr id)
            closure
        in
        let argt, p' = check_param p scope' tvs in
        let rest' = translate_type rest tvs in
        let fnt = Types.Fn (argt, rest') in
        let () = Hashtbl.add scope' self fnt in
        let body', bodyt = check_expr body scope' false in
        let () =
          force_type bodyt (specify rest')
            {
              v =
                Printf.sprintf "fn body must return %s, got %s"
                  (Types.t_to_str rest') (Types.t_to_str bodyt);
              span = body'.span;
            }
        in
        ({ v = Ir2.FnVal (p', closure, symcnt, body'); span }, fnt)
    | Ir1.FnCall (fn, arg) -> (
        let fn', fnt = check_expr fn symbols false in
        let arg', argt = check_expr arg symbols false in
        match fnt with
        | Types.Fn (paramt, rest) ->
            let assignments = Hashtbl.create 0 in
            let _ = unify paramt argt assignments span in
            let rest' = walk rest assignments in
            ({ v = Ir2.FnCall (fn', arg'); span }, rest')
        | _ ->
            raise
              (TypeError
                 {
                   v =
                     Format.sprintf "call of non-function, got %s"
                       (Types.t_to_str fnt);
                   span;
                 }))
    | Ir1.Bind (ptrn, expr) ->
        let e', t = check_expr expr symbols false in
        let ptrn' = match_ptrn_type ptrn span t symbols in
        ({ v = Ir2.Bind (ptrn', e'); span }, Types.Void)
    | Ir1.If (expr, body, body2) ->
        let e', t = check_expr expr symbols false in
        force_type t Types.Bool
          { v = "expected boolean, got " ^ Types.t_to_str t; span = expr.span };
        let scope' = Hashtbl.copy symbols in
        let b1, t1 = check_expr body scope' false in
        let scope' = Hashtbl.copy symbols in
        let b2, t2 = check_expr body2 scope' false in
        force_type t1 t2
          {
            v =
              Format.sprintf
                "expected same type for both if branches, got %s and %s"
                (Types.t_to_str t1) (Types.t_to_str t2);
            span;
          };
        ({ v = Ir2.If (e', b1, b2); span }, t1)
    | Ir1.Block body ->
        let scope' = Hashtbl.copy symbols in
        let body', t = check_exprs body scope' in
        ({ v = Ir2.Block body'; span }, t)
  in
  if chainhead then ({ v = Ir2.Do ir'; span = ir'.span }, t) else (ir', t)

and check_exprs es symbols =
  let len = Array.length es in
  let i = ref 1 in
  let t, acc' =
    Array.fold_left_map
      (fun t e ->
        let e2, t2 = check_expr e symbols (!i != len) in
        incr i;
        (Some t2, e2))
      None es
  in
  (acc', Option.get t)

let run (es, sc) =
  let symbols = Hashtbl.create 0 in
  List.iteri
    (fun i (bfn : Builtins.builtinFn) -> Hashtbl.add symbols i bfn.fnType)
    Builtins.builtins;
  let es', t = check_exprs es symbols in
  (es', sc)
