module Vars = Map.Make (String)

exception TypeError
exception NameError
exception MutError

type mutability = Const | Var
type 'a t = ('a * mutability) Vars.t

let empty = Vars.empty
let add_scope (scopes : 'a t list) : 'a t list = empty :: scopes

let rec search_scopes (scopes : 'a t list) (name : string) : 'a * mutability =
  match scopes with
  | [] -> raise NameError
  | scope :: scopes' -> (
      match Vars.find_opt name scope with
      | None -> search_scopes scopes' name
      | Some v -> v)

let rec update_scopes (scopes : 'a t list) (name : string) value : 'a t list =
  match scopes with
  | [] -> raise NameError
  | scope :: scopes' -> (
      match Vars.find_opt name scope with
      | None -> scope :: update_scopes scopes' name value
      | Some _ ->
          Vars.update name
            (fun v ->
              match v with
              | Some (_, Var) -> Some (value, Var)
              | None -> raise (Failure "Unreachable")
              | _ -> raise MutError)
            scope
          :: scopes')

let rec add_to_scope (scopes : 'a t list) (name : string) (m : mutability) value
    : 'a t list =
  match scopes with
  | [] -> raise (Failure "unreachable")
  | scope :: scopes' ->
      Vars.update name
        (fun v ->
          match (v, m) with
          | None, _ -> Some (value, m)
          | Some (_, Const), Const -> Some (value, m)
          | Some (_, Var), Var -> raise NameError
          | _ -> raise MutError)
        scope
      :: scopes'
