let handle_src (scope : int Eval.Vars.t) (src : string) =
  try
    let chars =
      src |> String.to_seq |> List.of_seq |> List.filter Char.Ascii.is_print
    in
    let tokens = Lexer.tokenize chars in
    let ast = Parser.parse tokens in
    let expr = Analyzer.analyze ast in
    Option.bind expr (fun expr -> Eval.eval scope expr)
  with
  | Failure s ->
      Printf.printf "error: %s\n" s;
      None
  | Eval.Quit -> exit 0

let rec main (scope : int Eval.Vars.t) =
  let () =
    output_string stdout "> ";
    flush stdout
  in
  let result = input_line stdin |> handle_src scope in
  let v, scope' =
    match result with
    | None -> (0, scope)
    | Some (v, scope') ->
        Printf.printf "%d\n" v;
        (v, scope')
  in
  main scope'

let () =
  print_endline ":help for help";
  main Eval.init_scope
