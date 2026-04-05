let handle_src scope src =
  let chars =
    src |> String.to_seq |> List.of_seq |> List.filter Char.Ascii.is_print
  in
  let tokens = Lexer.tokenize chars in
  let ast = Parser.parse tokens in
  let expr = match ast with None -> None | Some ast -> Analyzer.analyze ast in
  match expr with None -> None | Some expr -> Some (Eval.eval scope expr)

let rec main scope =
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

let () = main Eval.init_scope
