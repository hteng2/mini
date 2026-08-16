(* symresolver (ast -> ir1)
    symbols to ids
    determines Hashtbls *)

open Minic_lib

exception NameError of Errors.error

type resolve_ctx = {
  currtypes : (string, int) Hashtbl.t;
  exttypes : (string, int) Hashtbl.t list;
  nexttypeid : int ref;
  captures : (string, int) Hashtbl.t;
  currsymbols : (string, int) Hashtbl.t;
  extsymbols : (string, int) Hashtbl.t list;
  nextsymid : int ref;
}

let rec search_scopes scopes name =
  match scopes with
  | [] -> None
  | scope :: rest -> (
      match Hashtbl.find_opt scope name with
      | Some n -> Some n
      | None -> search_scopes rest name)

(* apply type induction rules and convert to ops *)
let rec resolve_expr ({ v; span } : Ast.expr) (ctx : resolve_ctx) : Ir1.expr =
  match v with
  (* atoms *)
  | Ast.Int n -> { v = Ir1.Int n; span }
  | Ast.Float n -> { v = Ir1.Float n; span }
  | Ast.Char c -> { v = Ir1.Char c; span }
  | Ast.Str s -> { v = Ir1.Str s; span }
  | Ast.Name name -> (
      match Hashtbl.find_opt ctx.currsymbols name with
      | Some n -> { v = Ir1.Name n; span }
      | None -> (
          match search_scopes ctx.extsymbols name with
          | Some n ->
              Hashtbl.add ctx.captures name n;
              { v = Ir1.Name n; span }
          | None -> raise (NameError { v = name; span })))
  | Ast.True -> { v = Ir1.Bool true; span }
  | Ast.False -> { v = Ir1.Bool false; span }
  | Ast.Void -> { v = Ir1.Void; span }
  (* unops *)
  | Ast.Neg e ->
      let ({ v = e' } as expr' : Ir1.expr) = resolve_expr e ctx in
      { v = Ir1.Neg expr'; span }
  | Ast.Pos e ->
      let ({ v = e' } as expr' : Ir1.expr) = resolve_expr e ctx in
      { v = Ir1.Pos expr'; span }
  | Ast.Not e ->
      let ({ v = e' } as expr' : Ir1.expr) = resolve_expr e ctx in
      { v = Ir1.Not expr'; span }
  (* biops *)
  | Ast.Eq (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = resolve_expr e1 ctx in
      let ({ v = e2' } as expr2' : Ir1.expr) = resolve_expr e2 ctx in
      { v = Ir1.Eq (expr1', expr2'); span }
  | Ast.Neq (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = resolve_expr e1 ctx in
      let ({ v = e2' } as expr2' : Ir1.expr) = resolve_expr e2 ctx in
      { v = Ir1.Neq (expr1', expr2'); span }
  | Ast.Gt (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = resolve_expr e1 ctx in
      let ({ v = e2' } as expr2' : Ir1.expr) = resolve_expr e2 ctx in
      { v = Ir1.Gt (expr1', expr2'); span }
  | Ast.Ge (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = resolve_expr e1 ctx in
      let ({ v = e2' } as expr2' : Ir1.expr) = resolve_expr e2 ctx in
      { v = Ir1.Ge (expr1', expr2'); span }
  | Ast.Lt (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = resolve_expr e1 ctx in
      let ({ v = e2' } as expr2' : Ir1.expr) = resolve_expr e2 ctx in
      { v = Ir1.Lt (expr1', expr2'); span }
  | Ast.Le (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = resolve_expr e1 ctx in
      let ({ v = e2' } as expr2' : Ir1.expr) = resolve_expr e2 ctx in
      { v = Ir1.Le (expr1', expr2'); span }
  | Ast.Add (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = resolve_expr e1 ctx in
      let ({ v = e2' } as expr2' : Ir1.expr) = resolve_expr e2 ctx in
      { v = Ir1.Add (expr1', expr2'); span }
  | Ast.Sub (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = resolve_expr e1 ctx in
      let ({ v = e2' } as expr2' : Ir1.expr) = resolve_expr e2 ctx in
      { v = Ir1.Sub (expr1', expr2'); span }
  | Ast.Mul (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = resolve_expr e1 ctx in
      let ({ v = e2' } as expr2' : Ir1.expr) = resolve_expr e2 ctx in
      { v = Ir1.Mul (expr1', expr2'); span }
  | Ast.Div (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = resolve_expr e1 ctx in
      let ({ v = e2' } as expr2' : Ir1.expr) = resolve_expr e2 ctx in
      { v = Ir1.Div (expr1', expr2'); span }
  | Ast.Mod (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = resolve_expr e1 ctx in
      let ({ v = e2' } as expr2' : Ir1.expr) = resolve_expr e2 ctx in
      { v = Ir1.Mod (expr1', expr2'); span }
  | Ast.And (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = resolve_expr e1 ctx in
      let ({ v = e2' } as expr2' : Ir1.expr) = resolve_expr e2 ctx in
      { v = Ir1.And (expr1', expr2'); span }
  | Ast.Or (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = resolve_expr e1 ctx in
      let ({ v = e2' } as expr2' : Ir1.expr) = resolve_expr e2 ctx in
      { v = Ir1.Or (expr1', expr2'); span }
  | Ast.Xor (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = resolve_expr e1 ctx in
      let ({ v = e2' } as expr2' : Ir1.expr) = resolve_expr e2 ctx in
      { v = Ir1.Xor (expr1', expr2'); span }
  | Ast.List es ->
      let rec infer_list es es_acc =
        match es with
        | [] -> es_acc
        | e :: es' ->
            let ({ v = e' } as expr1' : Ir1.expr) = resolve_expr e ctx in
            infer_list es' (expr1' :: es_acc)
      in
      let es' = infer_list es [] in
      { v = Ir1.List (List.rev es'); span }
  | Ast.At (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = resolve_expr e1 ctx in
      let ({ v = e2' } as expr2' : Ir1.expr) = resolve_expr e2 ctx in
      { v = Ir1.At (expr1', expr2'); span }
  | Ast.Tuple es ->
      let rec infer_list es es_acc =
        match es with
        | [] -> es_acc
        | e :: es' ->
            let ({ v = e' } as expr1' : Ir1.expr) = resolve_expr e ctx in
            infer_list es' (expr1' :: es_acc)
      in
      let es' = infer_list es [] in
      { v = Ir1.Tuple (List.rev es'); span }
  | Ast.FnVal (tvs, p, t, body) ->
      let ctx' =
        {
          currtypes = Hashtbl.create 0;
          exttypes = ctx.currtypes :: ctx.exttypes;
          nexttypeid = ctx.nexttypeid;
          captures = Hashtbl.create 0;
          currsymbols = Hashtbl.create 0;
          extsymbols = ctx.currsymbols :: ctx.extsymbols;
          nextsymid = ctx.nextsymid;
        }
      in
      let tvs' =
        List.map
          (fun tv ->
            let id = !(ctx.nexttypeid) in
            Hashtbl.add ctx'.currtypes tv id;
            incr ctx.nexttypeid;
            id)
          tvs
      in
      let self = !(ctx.nextsymid) in
      Hashtbl.add ctx'.currsymbols "self" self;
      incr ctx.nextsymid;
      let p' = resolve_param p ctx' in
      let t' = resolve_type t ctx' in
      let body' = resolve_expr body ctx' in
      let captures'' =
        Hashtbl.fold
          (fun name n acc ->
            (match Hashtbl.find_opt ctx.currsymbols name with
            | None -> Hashtbl.add ctx.captures name n
            | Some _ -> ());
            n :: acc)
          ctx'.captures []
      in
      { v = Ir1.FnVal (tvs', p', captures'', t', self, 0, body'); span }
  | Ast.FnCall (fn, arg) ->
      let fn' = resolve_expr fn ctx in
      let arg' = resolve_expr arg ctx in
      { v = Ir1.FnCall (fn', arg'); span }
  | Ast.Bind (ptrn, expr) ->
      let ({ v = e' } as expr' : Ir1.expr) = resolve_expr expr ctx in
      let ptrn' = resolve_pattern ptrn ctx.currsymbols ctx.nextsymid in
      { v = Ir1.Bind (ptrn', expr'); span }
  | Ast.If (expr, body, body2) ->
      let ({ v = e' } as expr' : Ir1.expr) = resolve_expr expr ctx in
      let ctx' =
        {
          currtypes = Hashtbl.create 0;
          exttypes = ctx.currtypes :: ctx.exttypes;
          nexttypeid = ctx.nexttypeid;
          captures = ctx.captures;
          currsymbols = Hashtbl.create 0;
          extsymbols = ctx.currsymbols :: ctx.extsymbols;
          nextsymid = ctx.nextsymid;
        }
      in
      let ({ v = e1' } as body' : Ir1.expr) = resolve_expr body ctx' in
      Hashtbl.iter
        (fun name n ->
          match Hashtbl.find_opt ctx.currsymbols name with
          | None -> Hashtbl.add ctx.captures name n
          | Some _ -> ())
        ctx'.captures;
      let ctx' =
        {
          currtypes = Hashtbl.create 0;
          exttypes = ctx.currtypes :: ctx.exttypes;
          nexttypeid = ctx.nexttypeid;
          captures = ctx.captures;
          currsymbols = Hashtbl.create 0;
          extsymbols = ctx.currsymbols :: ctx.extsymbols;
          nextsymid = ctx.nextsymid;
        }
      in
      let ({ v = e2' } as body2' : Ir1.expr) = resolve_expr body2 ctx' in
      Hashtbl.iter
        (fun name n ->
          match Hashtbl.find_opt ctx.currsymbols name with
          | None -> Hashtbl.add ctx.captures name n
          | Some _ -> ())
        ctx'.captures;
      { v = Ir1.If (expr', body', body2'); span }
  | Ast.Block (head, tail) ->
      let ctx' =
        {
          currtypes = Hashtbl.create 0;
          exttypes = ctx.currtypes :: ctx.exttypes;
          nexttypeid = ctx.nexttypeid;
          captures = Hashtbl.create 0;
          currsymbols = Hashtbl.create 0;
          extsymbols = ctx.currsymbols :: ctx.extsymbols;
          nextsymid = ctx.nextsymid;
        }
      in
      let head, tail = resolve_seq head tail ctx' in
      Hashtbl.iter
        (fun name n ->
          match Hashtbl.find_opt ctx.currsymbols name with
          | None -> Hashtbl.add ctx.captures name n
          | Some _ -> ())
        ctx'.captures;
      { v = Ir1.Block (head, tail); span }

and resolve_seq head tail ctx =
  let head' =
    head |> Queue.to_seq
    |> Seq.map (fun stmt ->
        match stmt with
        | Ast.Typedef (name, t) -> Ir1.Typedef (resolve_typedef name t ctx)
        | Ast.Expr expr -> Ir1.Expr (resolve_expr expr ctx))
    |> Array.of_seq
  in
  let tail' = resolve_expr tail ctx in
  (head', tail')

and resolve_param (p : Ast.param) (ctx : resolve_ctx) : Ir1.param =
  match p.v with
  | Ast.PrmUnit -> Ir1.PrmUnit
  | Ast.PrmLeaf (name, t) ->
      let i = !(ctx.nextsymid) in
      Hashtbl.add ctx.currsymbols name i;
      incr ctx.nextsymid;
      Ir1.PrmLeaf (i, resolve_type t ctx)
  | Ast.PrmTuple ps ->
      let ps' = List.map (fun p -> resolve_param p ctx) ps in
      Ir1.PrmTuple ps'

and resolve_type (t : Ast.parsed_type) (ctx : resolve_ctx) : Ir1.resolved_type =
  let v' =
    match t.v with
    | Ast.PtBase name ->
        Ir1.RtBase
          (match search_scopes (ctx.currtypes :: ctx.exttypes) name with
          | Some n -> n
          | None ->
              raise
                (NameError { v = "type name not found" ^ name; span = t.span }))
    | Ast.PtList t' -> Ir1.RtList (resolve_type t' ctx)
    | Ast.PtFn (t1, t2) -> Ir1.RtFn (resolve_type t1 ctx, resolve_type t2 ctx)
    | Ast.PtTup ts -> Ir1.RtTup (List.map (fun t -> resolve_type t ctx) ts)
  in
  { v = v'; span = t.span }

and resolve_pattern (p : Ast.pattern) symbols id : Ir1.pattern =
  match p with
  | Ast.PtrnUnit -> Ir1.PtrnUnit
  | Ast.PtrnLeaf name ->
      let i = !id in
      Hashtbl.add symbols name i;
      incr id;
      Ir1.PtrnLeaf i
  | Ast.PtrnTuple ps ->
      let ps' = List.map (fun p -> resolve_pattern p symbols id) ps in
      Ir1.PtrnTuple ps'

and resolve_typedef name t ctx =
  let t' = resolve_type t ctx in
  let id = !(ctx.nexttypeid) in
  Hashtbl.add ctx.currtypes name id;
  incr ctx.nexttypeid;
  (id, t')

(* apply type induction rules and convert to ops *)
let rec simplify_expr ({ v; span } as expr : Ir1.expr) symmapping id : Ir1.expr
    =
  match v with
  (* atoms *)
  | Ir1.Int _ | Ir1.Float _ | Ir1.Char _ | Ir1.Str _ | Ir1.Bool _ | Ir1.Void ->
      expr
  | Ir1.Name name -> { expr with v = Ir1.Name (Hashtbl.find symmapping name) }
  (* unops *)
  | Ir1.Neg e ->
      let ({ v = e' } as expr' : Ir1.expr) = simplify_expr e symmapping id in
      { v = Ir1.Neg expr'; span }
  | Ir1.Pos e ->
      let ({ v = e' } as expr' : Ir1.expr) = simplify_expr e symmapping id in
      { v = Ir1.Pos expr'; span }
  | Ir1.Not e ->
      let ({ v = e' } as expr' : Ir1.expr) = simplify_expr e symmapping id in
      { v = Ir1.Not expr'; span }
  (* biops *)
  | Ir1.Eq (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 symmapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 symmapping id in
      { v = Ir1.Eq (expr1', expr2'); span }
  | Ir1.Neq (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 symmapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 symmapping id in
      { v = Ir1.Neq (expr1', expr2'); span }
  | Ir1.Gt (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 symmapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 symmapping id in
      { v = Ir1.Gt (expr1', expr2'); span }
  | Ir1.Ge (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 symmapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 symmapping id in
      { v = Ir1.Ge (expr1', expr2'); span }
  | Ir1.Lt (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 symmapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 symmapping id in
      { v = Ir1.Lt (expr1', expr2'); span }
  | Ir1.Le (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 symmapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 symmapping id in
      { v = Ir1.Le (expr1', expr2'); span }
  | Ir1.Add (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 symmapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 symmapping id in
      { v = Ir1.Add (expr1', expr2'); span }
  | Ir1.Sub (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 symmapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 symmapping id in
      { v = Ir1.Sub (expr1', expr2'); span }
  | Ir1.Mul (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 symmapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 symmapping id in
      { v = Ir1.Mul (expr1', expr2'); span }
  | Ir1.Div (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 symmapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 symmapping id in
      { v = Ir1.Div (expr1', expr2'); span }
  | Ir1.Mod (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 symmapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 symmapping id in
      { v = Ir1.Mod (expr1', expr2'); span }
  | Ir1.And (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 symmapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 symmapping id in
      { v = Ir1.And (expr1', expr2'); span }
  | Ir1.Or (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 symmapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 symmapping id in
      { v = Ir1.Or (expr1', expr2'); span }
  | Ir1.Xor (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 symmapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 symmapping id in
      { v = Ir1.Xor (expr1', expr2'); span }
  | Ir1.List es ->
      let rec infer_list es es_acc =
        match es with
        | [] -> es_acc
        | e :: es' ->
            let ({ v = e' } as expr1' : Ir1.expr) =
              simplify_expr e symmapping id
            in
            infer_list es' (expr1' :: es_acc)
      in
      let es' = infer_list es [] in
      { v = Ir1.List (List.rev es'); span }
  | Ir1.At (e1, e2) ->
      let ({ v = e1' } as expr1' : Ir1.expr) = simplify_expr e1 symmapping id in
      let ({ v = e2' } as expr2' : Ir1.expr) = simplify_expr e2 symmapping id in
      { v = Ir1.At (expr1', expr2'); span }
  | Ir1.Tuple es ->
      let es' = List.map (fun e -> simplify_expr e symmapping id) es in
      { v = Ir1.Tuple es'; span }
  | Ir1.FnVal (tvs, p, captures, t, self, symcnt, body) ->
      let mapping' = Hashtbl.create 0 in
      let id' = ref 0 in

      let self' = !id' in
      Hashtbl.add mapping' self !id';
      incr id';

      let captures' =
        List.map (fun name -> Hashtbl.find symmapping name) captures
      in
      List.iter
        (fun name ->
          Hashtbl.add mapping' name !id';
          incr id')
        captures;

      let p' = simplify_param p mapping' id' in

      let body' = simplify_expr body mapping' id' in
      { v = Ir1.FnVal (tvs, p', captures', t, self', !id', body'); span }
  | Ir1.FnCall (fn, arg) ->
      let fn' = simplify_expr fn symmapping id in
      let arg' = simplify_expr arg symmapping id in
      { v = Ir1.FnCall (fn', arg'); span }
  | Ir1.Bind (ptrn, expr) ->
      let ({ v = e' } as expr' : Ir1.expr) = simplify_expr expr symmapping id in
      let ptrn' = simplify_pattern ptrn symmapping id in
      { v = Ir1.Bind (ptrn', expr'); span }
  | Ir1.If (expr, body, body2) ->
      let ({ v = e' } as expr' : Ir1.expr) = simplify_expr expr symmapping id in
      let ({ v = e1' } as body' : Ir1.expr) =
        simplify_expr body symmapping id
      in
      let ({ v = e2' } as body2' : Ir1.expr) =
        simplify_expr body2 symmapping id
      in
      { v = Ir1.If (expr', body', body2'); span }
  | Ir1.Block (head, tail) ->
      let head', tail' = simplify_seq head tail symmapping id in
      { v = Ir1.Block (head', tail'); span }

and simplify_seq head tail mapping id =
  let head' =
    Array.map
      (fun stmt ->
        match stmt with
        | Ir1.Typedef td -> stmt
        | Ir1.Expr expr -> Ir1.Expr (simplify_expr expr mapping id))
      head
  in
  let tail' = simplify_expr tail mapping id in
  (head', tail')

and simplify_param (p : Ir1.param) mapping id : Ir1.param =
  match p with
  | Ir1.PrmUnit -> Ir1.PrmUnit
  | Ir1.PrmLeaf (name, t) ->
      let i = !id in
      Hashtbl.add mapping name i;
      incr id;
      Ir1.PrmLeaf (i, t)
  | Ir1.PrmTuple ps ->
      let ps' = List.map (fun p -> simplify_param p mapping id) ps in
      Ir1.PrmTuple ps'

and simplify_pattern (p : Ir1.pattern) mapping id : Ir1.pattern =
  match p with
  | Ir1.PtrnUnit -> Ir1.PtrnUnit
  | Ir1.PtrnLeaf name ->
      let i = !id in
      Hashtbl.add mapping name i;
      incr id;
      Ir1.PtrnLeaf i
  | Ir1.PtrnTuple ps ->
      let ps' = List.map (fun p -> simplify_pattern p mapping id) ps in
      Ir1.PtrnTuple ps'

let run (head, tail) =
  let currtypes = Hashtbl.create 0 in
  let nexttypeid = ref 0 in
  List.iter
    (fun (t : Builtins.builtinType) ->
      Hashtbl.add currtypes t.name !nexttypeid;
      incr nexttypeid)
    Builtins.ts;

  let currsymbols = Hashtbl.create 0 in
  let nextsymid = ref 0 in
  List.iter
    (fun (bfn : Builtins.builtinFn) ->
      Hashtbl.add currsymbols bfn.name !nextsymid;
      incr nextsymid)
    Builtins.fns;

  let head', tail' =
    resolve_seq head tail
      {
        currtypes;
        exttypes = [];
        nexttypeid;
        captures = Hashtbl.create 0;
        currsymbols;
        extsymbols = [];
        nextsymid;
      }
  in
  let mapping = Hashtbl.create 0 in
  let nextsymid = ref 0 in
  List.iteri
    (fun i (bfn : Builtins.builtinFn) ->
      Hashtbl.add mapping i !nextsymid;
      incr nextsymid)
    Builtins.fns;
  let head'', tail'' = simplify_seq head' tail' mapping nextsymid in
  ((head'', tail''), !nextsymid)
