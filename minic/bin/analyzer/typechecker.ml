(* typechecker (ir1 -> ir2)
    type of exprs*)

open Minic_lib

exception TypeError of Errors.error

(* quickly check for type matching *)
let force_type t1 t2 e = if t1 = t2 then () else raise (TypeError e)

(* parsed types to real types *)
let rec translate_type (t : Ast.mini_type) =
  match t.v with
  | Ast.MtBase "int" -> Types.Int
  | Ast.MtBase "float" -> Types.Float
  | Ast.MtBase "bool" -> Types.Bool
  | Ast.MtBase "char" -> Types.Char
  | Ast.MtBase "void" -> Types.Void
  | Ast.MtBase s ->
      raise
        (TypeError
           { v = Printf.sprintf "unrecognized type %s" s; span = t.span })
  | Ast.MtList t' -> Types.List (translate_type t')
  | Ast.MtFn (t1, t2) -> Types.Fn (translate_type t1, translate_type t2)
  | Ast.MtTup ts -> (
      let ts' = List.map translate_type ts in
      match List.length ts' with
      | 0 -> Types.Void
      | 1 -> List.hd ts'
      | _ -> Types.Tuple (List.map translate_type ts))

(* check param type *)
let rec check_param (p : Ir1.param) (scope : (int, Types.t) Hashtbl.t) :
    Types.t * Ir2.pattern =
  match p with
  | Ir1.PrmUnit -> (Types.Void, Ir2.PtrnUnit)
  | Ir1.PrmLeaf (i, t) ->
      let t' = translate_type t in
      Hashtbl.add scope i t';
      (t', Ir2.PtrnLeaf i)
  | Ir1.PrmTuple ps ->
      let ts, ps' =
        ps |> List.map (fun p -> check_param p scope) |> List.split
      in
      (Types.Tuple ts, Ir2.PtrnTuple ps')

let rec match_ptrn_type (ptrn : Ir1.pattern) span (t : Types.t) scope :
    Ir2.pattern =
  match (ptrn, t) with
  | Ir1.PtrnUnit, Types.Void -> Ir2.PtrnUnit
  | Ir1.PtrnLeaf i, t ->
      Hashtbl.add scope i t;
      Ir2.PtrnLeaf i
  | Ir1.PtrnTuple ps, Types.Tuple ts when List.length ps = List.length ts ->
      let ps' = List.map2 (fun p t -> match_ptrn_type p span t scope) ps ts in
      Ir2.PtrnTuple ps'
  | _ -> raise (TypeError { v = "could not match pattern to type"; span })

(* apply type induction rules and convert to ops *)
let rec check_expr ({ v; span } : Ir1.expr) (scope : (int, Types.t) Hashtbl.t)
    chainhead : Ir2.expr * Types.t =
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
    | Ir1.Name name -> ({ v = Ir2.Name name; span }, Hashtbl.find scope name)
    | Ir1.Bool b -> ({ v = Ir2.Bool b; span }, Types.Bool)
    | Ir1.Void -> ({ v = Ir2.Void; span }, Types.Void)
    (* unops *)
    | Ir1.Neg e -> (
        let e', t = check_expr e scope false in
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
        let e', t = check_expr e scope false in
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
        let e', t = check_expr e scope false in
        force_type t Types.Bool
          {
            v = Format.sprintf "expected boolean, got %s" (Types.t_to_str t);
            span = e.span;
          };
        ({ v = Ir2.Not e'; span }, t)
    (* biops *)
    | Ir1.Eq (e1, e2) -> (
        let e1', t1 = check_expr e1 scope false in
        let e2', t2 = check_expr e2 scope false in
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
        let e1', t1 = check_expr e1 scope false in
        let e2', t2 = check_expr e2 scope false in
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
        let e1', t1 = check_expr e1 scope false in
        let e2', t2 = check_expr e2 scope false in
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
        let e1', t1 = check_expr e1 scope false in
        let e2', t2 = check_expr e2 scope false in
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
        let e1', t1 = check_expr e1 scope false in
        let e2', t2 = check_expr e2 scope false in
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
        let e1', t1 = check_expr e1 scope false in
        let e2', t2 = check_expr e2 scope false in
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
        let e1', t1 = check_expr e1 scope false in
        let e2', t2 = check_expr e2 scope false in
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
        let e1', t1 = check_expr e1 scope false in
        let e2', t2 = check_expr e2 scope false in
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
        let e1', t1 = check_expr e1 scope false in
        let e2', t2 = check_expr e2 scope false in
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
        let e1', t1 = check_expr e1 scope false in
        let e2', t2 = check_expr e2 scope false in
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
        let e1', t1 = check_expr e1 scope false in
        let e2', t2 = check_expr e2 scope false in
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
        let e1', t1 = check_expr e1 scope false in
        let e2', t2 = check_expr e2 scope false in
        force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
        force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
        ({ v = Ir2.And (e1', e2'); span }, Types.Bool)
    | Ir1.Or (e1, e2) ->
        let e1', t1 = check_expr e1 scope false in
        let e2', t2 = check_expr e2 scope false in
        force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
        force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
        ({ v = Ir2.Or (e1', e2'); span }, Types.Bool)
    | Ir1.Xor (e1, e2) ->
        let e1', t1 = check_expr e1 scope false in
        let e2', t2 = check_expr e2 scope false in
        force_type t1 Types.Bool { v = "expected boolean"; span = e1.span };
        force_type t2 Types.Bool { v = "expected boolean"; span = e1.span };
        ({ v = Ir2.Xor (e1', e2'); span }, Types.Bool)
    | Ir1.List es ->
        let rec infer_list es es_acc t =
          match es with
          | [] -> (t, es_acc)
          | e :: es' -> (
              let e', t1 = check_expr e scope false in
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
        let e1', t1 = check_expr e1 scope false in
        match t1 with
        | Types.List t ->
            let e2', t2 = check_expr e2 scope false in
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
          List.split (List.map (fun e -> check_expr e scope false) es)
        in
        ({ v = Ir2.Tuple es'; span }, Types.Tuple ts)
    | Ir1.FnVal (p, closure, rest, self, symcnt, body) ->
        let scope' = Hashtbl.create 0 in
        let id = ref 1 in
        let () =
          List.iter
            (fun name ->
              let t = Hashtbl.find scope name in
              Hashtbl.add scope' !id t;
              incr id)
            closure
        in
        let argt, p' = check_param p scope' in
        let rest' = translate_type rest in
        let fnt = Types.Fn (argt, rest') in
        let () = Hashtbl.add scope' self fnt in
        let body', bodyt = check_expr body scope' false in
        let () =
          force_type bodyt rest'
            {
              v =
                Printf.sprintf "fn body must return %s, got %s"
                  (Types.t_to_str rest') (Types.t_to_str bodyt);
              span = body'.span;
            }
        in
        ({ v = Ir2.FnVal (p', closure, symcnt, body'); span }, fnt)
    | Ir1.FnCall (fn, arg) -> (
        let fn', fnt = check_expr fn scope false in
        let arg', argt = check_expr arg scope false in
        match fnt with
        | Types.Fn (argt2, rest) ->
            if argt = argt2 then ({ v = Ir2.FnCall (fn', arg'); span }, rest)
            else
              raise
                (TypeError
                   {
                     v =
                       "argument type mismatch, expected "
                       ^ Types.t_to_str argt2 ^ ", got " ^ Types.t_to_str argt;
                     span;
                   })
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
        let e', t = check_expr expr scope false in
        let ptrn' = match_ptrn_type ptrn span t scope in
        ({ v = Ir2.Bind (ptrn', e'); span }, Types.Void)
    | Ir1.If (expr, body, body2) ->
        let e', t = check_expr expr scope false in
        force_type t Types.Bool
          { v = "expected boolean, got " ^ Types.t_to_str t; span = expr.span };
        let scope' = Hashtbl.copy scope in
        let b1, t1 = check_expr body scope' false in
        let scope' = Hashtbl.copy scope in
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
        let scope' = Hashtbl.copy scope in
        let body', t = check_exprs body scope' in
        ({ v = Ir2.Block body'; span }, t)
  in
  if chainhead then ({ v = Ir2.Do ir'; span = ir'.span }, t) else (ir', t)

and check_exprs es scopes =
  let len = Array.length es in
  let i = ref 1 in
  let t, acc' =
    Array.fold_left_map
      (fun t e ->
        let e2, t2 = check_expr e scopes (!i != len) in
        incr i;
        (Some t2, e2))
      None es
  in
  (acc', Option.get t)

let run (es, sc) =
  let s = Hashtbl.create 0 in
  List.iteri
    (fun i (bfn : Builtins.builtinFn) -> Hashtbl.add s i bfn.fnType)
    Builtins.builtins;
  let es', t = check_exprs es s in
  (es', sc)
