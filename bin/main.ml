let f x = ()

let handle_src (src : char Stream.t) : unit =
  try
    let tokens = Lexer.tokenize src in
    (* let tokens = Debug.print_tokens tokens in *)
    let p = Parser.parse tokens in
    let () = Debug.print_p p 0 in
    let _ = Analyzer.analyze p in
    let _ = Eval.eval p (Scopes.add_scope []) in
    ()
  with
  | Errors.Expected { v; span = (sr, sc), (er, ec) } ->
      Printf.printf "error: %d:%d-%d:%d - expected %s\n" sr sc er ec v
  | Errors.Unexpected { v; span = (sr, sc), (er, ec) } ->
      Printf.printf "error: %d:%d-%d:%d - unexpected %s\n" sr sc er ec v
  | Errors.TypeError { v; span = (sr, sc), (er, ec) } ->
      Printf.printf "type error: %d:%d-%d:%d %s\n" sr sc er ec v
  | Eval.Div span -> Printf.printf "exception: division by 0\n"
  | Eval.Range span -> Printf.printf "exception: out of range\n"
  | Scopes.NameError n -> Printf.printf "exception: unrecognized name %s\n" n

let rec src_to_stream src =
  Stream.push (fun () ->
      match In_channel.input_char src with
      | Some c -> Head (c, src_to_stream src)
      | None -> End)

let () =
  if Array.length Sys.argv <> 2 then
    print_endline "expected argument <filename>"
  else
    let file = open_in Sys.argv.(1) in
    let src = src_to_stream file in
    handle_src src
