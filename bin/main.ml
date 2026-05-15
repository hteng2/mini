let handle_src (src : string) : unit =
  try
    let chars = src |> String.to_seq |> List.of_seq in
    let tokens = Lexer.tokenize chars in
    let () = Lexer.print_tokens tokens in
    let decs = Parser.parse tokens in
    let _ = Analyzer.analyze decs in
    let _ = Eval.eval decs [ Scopes.empty ] in
    ()
  with
  | Errors.Expected { v; span = (sr, sc), (er, ec) } ->
      Printf.printf "error: %d:%d-%d:%d - expected %s\n" sr sc er ec v
  | Errors.Unexpected { v; span = (sr, sc), (er, ec) } ->
      Printf.printf "error: %d:%d-%d:%d - unexpected %s\n" sr sc er ec v
  | Eval.Div_by_0 -> Printf.printf "exception: division by 0\n"
  | Scopes.NameError -> Printf.printf "exception: unrecognized name\n"
  | Failure s -> print_endline s

let () =
  print_newline ();
  if Array.length Sys.argv <> 2 then
    print_endline "expected argument <filename>"
  else
    let file = open_in Sys.argv.(1) in
    let src = In_channel.input_all file in
    handle_src src
