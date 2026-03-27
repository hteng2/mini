let handle_src src =
  let chars = String.fold_right (fun c acc -> c::acc) src [] in
  let valid_chars = List.filter Char.Ascii.is_print chars in
  let tokens = Lexer.tokenize valid_chars in
  let () = Lexer.print_tokens tokens in
  let ast = Parser.parse tokens (fun (_, x) -> x) in
  match ast with
    Some ast -> Parser.print_ast ast
  | None -> print_endline "failed"

let rec main () =
  let () = output_string stdout "> " in
  let () = flush stdout in
  let src = input_line stdin in
  (handle_src src; main ())

let () = main ()
