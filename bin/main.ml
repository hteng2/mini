let f x = ()

let handle_src (src : char Stream.t) : unit =
  try
    let p =
      src |> Lexer.tokenize
      (* |> Debug.print_tokens *)
      |> Parser.parse
    in
    Debug.print_p p 0;
    let _, ir = Analyzer.analyze p in
    print_endline "ok";
    Eval.eval ir (Closure.empty () :: [])
  with
  | Errors.Expected { v; span = (sr, sc), (er, ec) } ->
      Printf.printf "error: %d:%d-%d:%d - expected %s\n" sr sc er ec v
  | Errors.Unexpected { v; span = (sr, sc), (er, ec) } ->
      Printf.printf "error: %d:%d-%d:%d - unexpected %s\n" sr sc er ec v
  | Errors.TypeError { v; span = (sr, sc), (er, ec) } ->
      Printf.printf "type error: %d:%d-%d:%d %s\n" sr sc er ec v
  | Eval.Div -> Printf.printf "exception: division by 0\n"
  | Eval.Range -> Printf.printf "exception: out of range\n"

let rec src_to_stream src =
  Stream.push (fun () ->
      match In_channel.input_char src with
      | Some c -> Head (c, src_to_stream src)
      | None -> End)

let () =
  print_newline ();
  if Array.length Sys.argv <> 2 then
    print_endline "expected argument <filename>"
  else
    let file = open_in Sys.argv.(1) in
    let src = src_to_stream file in
    handle_src src
