let handle_src scope src =
  let chars = String.fold_right (fun c acc -> c :: acc) src [] in
  let valid_chars = List.filter Char.Ascii.is_print chars in
  let tokens = Lexer.tokenize valid_chars in
  let () = Lexer.print_tokens tokens in
  let ast = Parser.parse tokens (fun (_, x) -> x) in
  let () =
    match ast with
    | Some ast -> Parser.print_ast ast
    | None -> print_endline "failed"
  in
  let expr =
    match ast with
    | None -> None
    | Some ast -> Analyzer.analyze ast
  in
  let () =
    match expr with
    | Some expr -> Analyzer.print_expr expr
    | None -> print_endline "failed"
  in
  match expr with
  | None -> None
  | Some expr -> Some (Eval.eval scope expr)
;;

let rec main scope =
  let () = output_string stdout "> " in
  let () = flush stdout in
  let src = input_line stdin in
  let result = handle_src scope src in
  let v, scope' =
    match result with
    | None -> 0, scope
    | Some (v, scope') -> v, scope'
  in
  let () = Printf.printf "%d\n" v in
  main scope'
;;

let () = main Eval.init_scope
