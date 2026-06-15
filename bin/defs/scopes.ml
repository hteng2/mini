module Vars = Hashtbl.Make (String)

exception NameError

type 'a t = 'a Vars.t

let add_scope (scopes : 'a t list) : 'a t list = Vars.create 0 :: scopes

let pop_scope (scopes : 'a t list) : 'a t list =
  match scopes with
  | [] -> raise (Invalid_argument "empty scopes")
  | _ :: scopes' -> scopes'

let search_top (scopes : 'a t list) (name : string) : 'a option =
  match scopes with
  | [] -> assert false
  | scope :: _ -> Vars.find_opt scope name

let rec search_scopes (scopes : 'a t list) (name : string) : 'a =
  match scopes with
  | [] -> raise NameError
  | scope :: scopes' -> (
      match Vars.find_opt scope name with
      | None -> search_scopes scopes' name
      | Some v -> v)

let rec update_scopes (scopes : 'a t list) (name : string) value : unit =
  match scopes with
  | [] -> raise NameError
  | scope :: scopes' -> (
      match Vars.find_opt scope name with
      | None -> update_scopes scopes' name value
      | Some _ -> Vars.replace scope name value)

let rec add_to_scope (scopes : 'a t list) (name : string) value : unit =
  match scopes with
  | [] -> raise (Invalid_argument "empty scopes")
  | scope :: scopes' -> Vars.add scope name value
