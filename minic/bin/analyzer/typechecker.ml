(* typechecker (ir1 -> ir2)
    type of exprs*)

open Minic_lib

(* quickly check for type matching *)
let force_type t1 t2 e = if t1 = t2 then () else raise (Types.TypeError e)

(* convert param parsed types to actual types *)
let rec translate_type (t : Ir1.resolved_type) tds =
  match t.v with
  | Ir1.RtBase s -> Hashtbl.find tds s
  | Ir1.RtList t' -> Types.List (translate_type t' tds)
  | Ir1.RtFn (t1, t2) -> Types.Fn (translate_type t1 tds, translate_type t2 tds)
  | Ir1.RtTup ts -> (
      let ts' = List.map (fun t -> translate_type t tds) ts in
      match List.length ts' with
      | 0 -> Types.Void
      | 1 -> List.hd ts'
      | _ -> Types.Tuple ts')

(* Types.specializes types in symbols *)
let rec check_param prm symbols tds =
  match prm with
  | Ir1.PrmUnit -> (Types.Void, Ir2.PtrnUnit)
  | Ir1.PrmLeaf (i, t) ->
      let t' = translate_type t tds in
      Hashtbl.add symbols i (Types.specialize t');
      (t', Ir2.PtrnLeaf i)
  | Ir1.PrmTuple ps ->
      let ts, ps' =
        ps |> List.map (fun p -> check_param p symbols tds) |> List.split
      in
      (Types.Tuple ts, Ir2.PtrnTuple ps')

let rec match_ptrn_type ptrn span t symbols : Ir2.pattern =
  match (ptrn, t) with
  | Ir1.PtrnUnit, Types.Void -> Ir2.PtrnUnit
  | Ir1.PtrnLeaf i, t ->
      Hashtbl.add symbols i t;
      Ir2.PtrnLeaf i
  | Ir1.PtrnTuple ps, Types.Tuple ts when List.length ps = List.length ts ->
      let ps' = List.map2 (fun p t -> match_ptrn_type p span t symbols) ps ts in
      Ir2.PtrnTuple ps'
  | _ -> raise (Types.TypeError { v = "could not match pattern to type"; span })

(* apply type induction rules and convert to ops *)
let rec check_expr ({ v; span } : Ir1.expr) typedefs symbols chainhead =
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
        let e', t = check_expr e typedefs symbols false in
        match t with
        | Types.Int -> ({ v = Ir2.INeg e'; span }, t)
        | Types.Float -> ({ v = Ir2.FNeg e'; span }, t)
        | _ ->
            raise
              (Types.TypeError
                 {
                   v =
                     Format.sprintf "expected int or float, got %s"
                       (Types.t_to_str t);
                   span = e.span;
                 }))
    | Ir1.Pos e -> (
        let e', t = check_expr e typedefs symbols false in
        match t with
        | Types.Int -> (e', t)
        | Types.Float -> (e', t)
        | _ ->
            raise
              (Types.TypeError
                 {
                   v =
                     Format.sprintf "expected int or float, got %s"
                       (Types.t_to_str t);
                   span = e.span;
                 }))
    | Ir1.Not e ->
        let e', t = check_expr e typedefs symbols false in
        force_type t Types.Bool
          {
            v = Format.sprintf "expected boolean, got %s" (Types.t_to_str t);
            span = e.span;
          };
        ({ v = Ir2.Not e'; span }, t)
    (* biops *)
    | Ir1.Eq (e1, e2) -> (
        let e1', t1 = check_expr e1 typedefs symbols false in
        let e2', t2 = check_expr e2 typedefs symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.IEq (e1', e2'); span }, Types.Bool)
        | Types.Bool, Types.Bool ->
            ({ v = Ir2.BEq (e1', e2'); span }, Types.Bool)
        | Types.Char, Types.Char ->
            ({ v = Ir2.CEq (e1', e2'); span }, Types.Bool)
        | _ ->
            raise
              (Types.TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int = int, bool = bool, or char = char, got \
                        %s = %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.Neq (e1, e2) -> (
        let e1', t1 = check_expr e1 typedefs symbols false in
        let e2', t2 = check_expr e2 typedefs symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.INeq (e1', e2'); span }, Types.Bool)
        | Types.Bool, Types.Bool ->
            ({ v = Ir2.BEq (e1', e2'); span }, Types.Bool)
        | Types.Char, Types.Char ->
            ({ v = Ir2.CNeq (e1', e2'); span }, Types.Bool)
        | _ ->
            raise
              (Types.TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int != int, bool != bool, or char != char, \
                        got %s != %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.Gt (e1, e2) -> (
        let e1', t1 = check_expr e1 typedefs symbols false in
        let e2', t2 = check_expr e2 typedefs symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.IGt (e1', e2'); span }, Types.Bool)
        | Types.Float, Types.Float ->
            ({ v = Ir2.FGt (e1', e2'); span }, Types.Bool)
        | Types.Char, Types.Char ->
            ({ v = Ir2.CGt (e1', e2'); span }, Types.Bool)
        | _ ->
            raise
              (Types.TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int > int, float > float, or char > char, got \
                        %s > %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.Ge (e1, e2) -> (
        let e1', t1 = check_expr e1 typedefs symbols false in
        let e2', t2 = check_expr e2 typedefs symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.IGe (e1', e2'); span }, Types.Bool)
        | Types.Char, Types.Char ->
            ({ v = Ir2.CGe (e1', e2'); span }, Types.Bool)
        | _ ->
            raise
              (Types.TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int >= int, float >= float, or char >= char, \
                        got %s >= %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.Lt (e1, e2) -> (
        let e1', t1 = check_expr e1 typedefs symbols false in
        let e2', t2 = check_expr e2 typedefs symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.ILt (e1', e2'); span }, Types.Bool)
        | Types.Float, Types.Float ->
            ({ v = Ir2.FLt (e1', e2'); span }, Types.Bool)
        | Types.Char, Types.Char ->
            ({ v = Ir2.CLt (e1', e2'); span }, Types.Bool)
        | _ ->
            raise
              (Types.TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int < int, float < float, or char < char, got \
                        %s < %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.Le (e1, e2) -> (
        let e1', t1 = check_expr e1 typedefs symbols false in
        let e2', t2 = check_expr e2 typedefs symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.ILe (e1', e2'); span }, Types.Bool)
        | Types.Char, Types.Char ->
            ({ v = Ir2.CLe (e1', e2'); span }, Types.Bool)
        | _ ->
            raise
              (Types.TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int < int, float < float, or char < char, got \
                        %s < %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.Add (e1, e2) -> (
        let e1', t1 = check_expr e1 typedefs symbols false in
        let e2', t2 = check_expr e2 typedefs symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.IAdd (e1', e2'); span }, t1)
        | Types.Float, Types.Float -> ({ v = Ir2.FAdd (e1', e2'); span }, t1)
        | _ ->
            raise
              (Types.TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int + int or float + float, got %s + %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.Sub (e1, e2) -> (
        let e1', t1 = check_expr e1 typedefs symbols false in
        let e2', t2 = check_expr e2 typedefs symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.ISub (e1', e2'); span }, t1)
        | Types.Float, Types.Float -> ({ v = Ir2.FSub (e1', e2'); span }, t1)
        | _ ->
            raise
              (Types.TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int - int or float - float, got %s - %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.Mul (e1, e2) -> (
        let e1', t1 = check_expr e1 typedefs symbols false in
        let e2', t2 = check_expr e2 typedefs symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.IMul (e1', e2'); span }, t1)
        | Types.Float, Types.Float -> ({ v = Ir2.FMul (e1', e2'); span }, t1)
        | _ ->
            raise
              (Types.TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int * int or float * float, got %s * %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.Div (e1, e2) -> (
        let e1', t1 = check_expr e1 typedefs symbols false in
        let e2', t2 = check_expr e2 typedefs symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.IDiv (e1', e2'); span }, t1)
        | Types.Float, Types.Float -> ({ v = Ir2.FDiv (e1', e2'); span }, t1)
        | _ ->
            raise
              (Types.TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int / int or float / float, got %s / %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.Mod (e1, e2) -> (
        let e1', t1 = check_expr e1 typedefs symbols false in
        let e2', t2 = check_expr e2 typedefs symbols false in
        match (t1, t2) with
        | Types.Int, Types.Int -> ({ v = Ir2.IMod (e1', e2'); span }, t1)
        | _ ->
            raise
              (Types.TypeError
                 {
                   v =
                     Format.sprintf
                       "expected int %% int or float %% float, got %s %s"
                       (Types.t_to_str t1) (Types.t_to_str t2);
                   span;
                 }))
    | Ir1.And (e1, e2) ->
        let e1', t1 = check_expr e1 typedefs symbols false in
        let e2', t2 = check_expr e2 typedefs symbols false in
        force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
        force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
        ({ v = Ir2.And (e1', e2'); span }, Types.Bool)
    | Ir1.Or (e1, e2) ->
        let e1', t1 = check_expr e1 typedefs symbols false in
        let e2', t2 = check_expr e2 typedefs symbols false in
        force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
        force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
        ({ v = Ir2.Or (e1', e2'); span }, Types.Bool)
    | Ir1.Xor (e1, e2) ->
        let e1', t1 = check_expr e1 typedefs symbols false in
        let e2', t2 = check_expr e2 typedefs symbols false in
        force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
        force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
        ({ v = Ir2.Xor (e1', e2'); span }, Types.Bool)
    | Ir1.List es ->
        let rec infer_list es es_acc t_left =
          match es with
          | [] -> (t_left, es_acc)
          | e :: es' ->
              let e', t = check_expr e typedefs symbols false in
              if t_left = Types.Any || t_left = t then
                infer_list es' (e' :: es_acc) t
              else
                raise
                  (Types.TypeError
                     {
                       v =
                         Format.sprintf "expected %s, got %s"
                           (Types.t_to_str t_left) (Types.t_to_str t);
                       span;
                     })
        in
        let t, es' = infer_list es [] Any in
        ({ v = Ir2.List (Array.of_list (List.rev es')); span }, Types.List t)
    | Ir1.At (e1, e2) -> (
        let e1', t1 = check_expr e1 typedefs symbols false in
        match t1 with
        | Types.List t ->
            let e2', t2 = check_expr e2 typedefs symbols false in
            force_type t2 Types.Int { v = "expected integer"; span = e2.span };
            ({ v = Ir2.At (e1', e2'); span }, t)
        | _ ->
            raise
              (Types.TypeError
                 {
                   v = Format.sprintf "list access of %s" (Types.t_to_str t1);
                   span;
                 }))
    | Ir1.Tuple es ->
        let es', ts =
          List.split
            (List.map (fun e -> check_expr e typedefs symbols false) es)
        in
        ({ v = Ir2.Tuple es'; span }, Types.Tuple ts)
    | Ir1.FnVal (tvs, p, closure, rest, self, symcnt, body) ->
        List.iter (fun i -> Hashtbl.add typedefs i (Types.All i)) tvs;

        let symbols' = Hashtbl.create 0 in
        let id = ref 1 in
        List.iter
          (fun name ->
            let t = Hashtbl.find symbols name in
            Hashtbl.add symbols' !id t;
            incr id)
          closure;

        (* outward type *)
        let argt, p' = check_param p symbols' typedefs in
        let rest' = translate_type rest typedefs in
        let fnt = Types.Fn (argt, rest') in

        (* inward type *)
        let rest_inward = Types.specialize rest' in
        let fnt_inward = Types.specialize fnt in
        let () = Hashtbl.add symbols' self fnt_inward in
        let body', bodyt = check_expr body typedefs symbols' false in
        let () =
          force_type bodyt rest_inward
            {
              v =
                Printf.sprintf "fn body must return %s, got %s"
                  (Types.t_to_str rest') (Types.t_to_str bodyt);
              span = body'.span;
            }
        in
        ({ v = Ir2.FnVal (p', closure, symcnt, body'); span }, fnt)
    | Ir1.FnCall (fn, arg) -> (
        let fn', fnt = check_expr fn typedefs symbols false in
        let arg', argt = check_expr arg typedefs symbols false in
        match fnt with
        | Types.Fn (paramt, rest) ->
            let assignments = Hashtbl.create 0 in
            let _ = Types.unify paramt argt assignments span in
            let rest' = Types.walk rest assignments in
            ({ v = Ir2.FnCall (fn', arg'); span }, rest')
        | _ ->
            raise
              (Types.TypeError
                 {
                   v =
                     Format.sprintf "call of non-function, got %s"
                       (Types.t_to_str fnt);
                   span;
                 }))
    | Ir1.Bind (ptrn, expr) ->
        let e', t = check_expr expr typedefs symbols false in
        let ptrn' = match_ptrn_type ptrn span t symbols in
        ({ v = Ir2.Bind (ptrn', e'); span }, Types.Void)
    | Ir1.If (expr, body, body2) ->
        let e', t = check_expr expr typedefs symbols false in
        force_type t Types.Bool
          { v = "expected boolean, got " ^ Types.t_to_str t; span = expr.span };
        let scope' = Hashtbl.copy symbols in
        let b1, t1 = check_expr body typedefs scope' false in
        let scope' = Hashtbl.copy symbols in
        let b2, t2 = check_expr body2 typedefs scope' false in
        force_type t1 t2
          {
            v =
              Format.sprintf
                "expected same type for both if branches, got %s and %s"
                (Types.t_to_str t1) (Types.t_to_str t2);
            span;
          };
        ({ v = Ir2.If (e', b1, b2); span }, t1)
    | Ir1.Block (head, tail) ->
        let scope' = Hashtbl.copy symbols in
        let exprs, t = check_exprs (head, tail) typedefs scope' in
        ({ v = Ir2.Block exprs; span }, t)
  in
  if chainhead then ({ v = Ir2.Do ir'; span = ir'.span }, t) else (ir', t)

and check_exprs (head, tail) typedefs symbols =
  let head' =
    head
    |> Array.fold_left
         (fun acc stmt ->
           match stmt with
           | Ir1.Typedef (i, rt) ->
               let t = translate_type rt typedefs in
               Hashtbl.add typedefs i t;
               None :: acc
           | Ir1.Expr expr ->
               let expr', _ = check_expr expr typedefs symbols true in
               Some expr' :: acc)
         []
    |> List.filter_map (fun x -> x)
  in
  let tail', t = check_expr tail typedefs symbols false in
  let exprs = List.rev (tail' :: head') in
  (exprs |> Array.of_list, t)

let run ((head, tail), sc) =
  let typedefs = Hashtbl.create 0 in
  List.iteri
    (fun i (t : Builtins.builtinType) -> Hashtbl.add typedefs i t.t)
    Builtins.ts;
  let symbols = Hashtbl.create 0 in
  List.iteri
    (fun i (bfn : Builtins.builtinFn) -> Hashtbl.add symbols i bfn.fnType)
    Builtins.fns;
  let es', t = check_exprs (head, tail) typedefs symbols in
  (es', sc)
