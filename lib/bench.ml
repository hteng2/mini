let create title =
  let records = Queue.create () in
  let current = ref None in
  let start name = current := Some (Sys.time (), name) in
  let stop () =
    let now = Sys.time () in
    match !current with
    | None -> assert false
    | Some (start, name) -> Queue.add (Float.sub now start, name) records
  in
  let print () =
    print_endline title;
    Queue.iter
      (fun (time, name) -> Printf.printf "%0.6f %s\n" time name)
      records
  in
  (start, stop, print)
