module Vars = Map.Make (String)

exception NameError

type 'a t = 'a Vars.t

let empty = Vars.empty
let add_scope (scopes : 'a t list) : 'a t list = empty :: scopes

let search_top (scopes : 'a t list) (name : string) : 'a option =
  match scopes with
  | [] -> assert false
  | scope :: _ -> Vars.find_opt name scope

let rec search_scopes (scopes : 'a t list) (name : string) : 'a =
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
      | Some _ -> Vars.add name value scope :: scopes')

let rec add_to_scope (scopes : 'a t list) (name : string) value : 'a t list =
  match scopes with
  | [] -> assert false
  | scope :: scopes' -> Vars.add name value scope :: scopes'
