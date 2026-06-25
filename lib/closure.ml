module H = Hashtbl.Make (String)

type 'a t = 'a H.t

let empty _ = H.create 0
let copy c = H.copy c
let get c name = H.find_opt c name

let set c name v =
  match get c name with None -> H.add c name v | Some _ -> H.replace c name v

let del c name = H.remove c name
let merge c1 c2 = H.iter (fun name v -> set c1 name v) c2
let iter f c1 = H.iter f c1

let search cs name =
  List.fold_left
    (fun acc c -> match acc with Some _ -> acc | None -> get c name)
    None cs
