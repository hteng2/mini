let f x = ()

let handle_src (src : string) : unit =
  try
    let chars = src |> String.to_seq |> List.of_seq in
    let tokens = Lexer.tokenize chars in
    (* let () = Debug.print_tokens tokens in *)
    let p = Parser.parse tokens in
    (* let () = Debug.print_p p 0 in *)
    let _ = Analyzer.analyze p in
    let _ = Eval.eval p (Scopes.add_scope []) in
    ()
  with
  | Errors.Expected { v; span = (sr, sc), (er, ec) } ->
      Printf.printf "error: %d:%d-%d:%d - expected %s\n" sr sc er ec v
  | Errors.Unexpected { v; span = (sr, sc), (er, ec) } ->
      Printf.printf "error: %d:%d-%d:%d - unexpected %s\n" sr sc er ec v
  | Errors.TypeError { v; span = (sr, sc), (er, ec) } ->
      Printf.printf "type error: %d:%d-%d:%d\n" sr sc er ec
  | Eval.Div span -> Printf.printf "exception: division by 0\n"
  | Eval.Range span -> Printf.printf "exception: out of range\n"
  | Scopes.NameError -> Printf.printf "exception: unrecognized name\n"

let () =
  if Array.length Sys.argv <> 2 then
    print_endline "expected argument <filename>"
  else
    let file = open_in Sys.argv.(1) in
    let src = In_channel.input_all file in
    handle_src src
