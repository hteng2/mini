exception TypeError of Errors.error

type t =
  | Any
  | All of int
  | Some of int
  | Int
  | Float
  | Bool
  | Char
  | Void
  | List of t
  | Fn of t * t
  | Tuple of t list

let rec t_to_str (t : t) : string =
  match t with
  | Any -> "any"
  | All n -> Printf.sprintf "all %d" n
  | Some n -> Printf.sprintf "some %d" n
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Char -> "char"
  | Void -> "void"
  | List t' -> t_to_str t' ^ "[]"
  | Fn (t1, t2) -> Printf.sprintf "%s -> %s" (t_to_str t1) (t_to_str t2)
  | Tuple ts ->
      Printf.sprintf "(%s)" (String.concat ", " (List.map t_to_str ts))

(* HM unification *)
let rec walk t assignments : t =
  match t with
  | All n -> (
      match Hashtbl.find_opt assignments n with
      | None -> t
      | Some t' -> walk t' assignments)
  | _ -> t

let unify t0 t1 assignments span =
  let rec occurs n t =
    match walk t assignments with
    | All n' -> n == n'
    | Any | Some _ | Int | Float | Bool | Char | Void -> false
    | List t' -> occurs n t'
    | Fn (f, t) -> occurs n f || occurs n t
    | Tuple ts -> List.fold_left (fun acc t -> acc || occurs n t) false ts
  in
  let assign n t =
    let t' = walk t assignments in
    if occurs n t then
      raise
        (TypeError
           {
             v =
               Printf.sprintf "failed to unify: %s occurs in %s (from %s)"
                 (t_to_str (All n)) (t_to_str t') (t_to_str t);
             span;
           })
    else Hashtbl.replace assignments n t'
  in
  let rec helper t0 t1 =
    let t0', t1' = (walk t0 assignments, walk t1 assignments) in
    match (t0', t1') with
    | Any, _ -> t1'
    | _, Any -> t0'
    | All n0, All n1 when n0 = n1 -> t0'
    | All n0, _ ->
        assign n0 t1';
        t1'
    | _, All n1 ->
        assign n1 t0';
        t0'
    | Some n0, Some n1 ->
        if n0 = n1 then t0'
        else
          raise
            (TypeError
               {
                 v =
                   Printf.sprintf "failed to unify %s with %s" (t_to_str t0)
                     (t_to_str t1);
                 span;
               })
    | Int, Int | Float, Float | Bool, Bool | Char, Char | Void, Void -> t0
    | List inner0, List inner1 -> List (helper inner0 inner1)
    | Fn (from0, to0), Fn (from1, to1) -> Fn (helper from0 from1, helper to0 to1)
    | Tuple ts0, Tuple ts1 ->
        Tuple (List.map2 (fun t0 t1 -> helper t0 t1) ts0 ts1)
    | _ ->
        raise
          (TypeError
             {
               v =
                 Printf.sprintf "failed to unify %s with %s" (t_to_str t0)
                   (t_to_str t1);
               span;
             })
  in
  helper t0 t1

let rec specialize t =
  match t with
  | All n -> Some n
  | List t -> List (specialize t)
  | Fn (f, t) -> Fn (specialize f, specialize t)
  | Tuple ts -> Tuple (List.map specialize ts)
  | t -> t
