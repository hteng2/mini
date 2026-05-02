let handle_src (src : string) : unit =
  try
    let chars = src |> String.to_seq |> List.of_seq in
    let tokens = Lexer.tokenize chars in
    let decs = Parser.parse tokens in
    let typed_decs = Analyzer.analyze decs in
    let _ = Eval.eval typed_decs [ Scopes.empty ] in
    ()
  with
  | Eval.Div_by_0 -> Printf.printf "exception: division by 0\n"
  | Scopes.NameError -> Printf.printf "exception: unrecognized name\n"
  | Failure s -> print_endline s

let () =
  if Array.length Sys.argv <> 2 then
    print_endline "expected argument <filename>"
  else
    let file = open_in Sys.argv.(1) in
    let src = In_channel.input_all file in
    handle_src src
