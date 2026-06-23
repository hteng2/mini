let f x = ()

let rec src_to_stream src =
 fun () ->
  match In_channel.input_char src with
  | Some c -> Stream.Head (c, src_to_stream src)
  | None -> Stream.End

let rec stream_concat ss =
  match ss with
  | [] -> fun () -> Stream.End
  | s :: ss' -> (
      fun () ->
        match s () with
        | Stream.Head (x, s') -> Stream.Head (x, stream_concat (s' :: ss'))
        | Stream.End -> (stream_concat ss') ())

let rec parse_and_import file files =
  if List.mem file files then assert false
  else
    try
      let src = open_in file |> src_to_stream in
      let ts = Lexer.tokenize src file in
      let ts', imports = Parser.scan_imports ts in
      let files' = file :: files in
      let asts, files'' =
        List.fold_left
          (fun (asts_acc, files) import ->
            if List.mem import files then
              raise (Failure "circular import detected")
            else
              let ast, files' = parse_and_import import files in
              (ast :: asts_acc, files'))
          ([], files') imports
      in
      let ast = Parser.parse ts' in
      let ast' = stream_concat (asts @ [ ast ]) in
      (ast', files'')
    with
    | Errors.Expected { v; span = sn, (a, b), (c, d) } ->
        Printf.printf "error: %s %d:%d-%d:%d - expected %s\n" sn a b c d v;
        exit 0
    | Errors.Unexpected { v; span = sn, (a, b), (c, d) } ->
        Printf.printf "error: %s %d:%d-%d:%d - unexpected %s\n" sn a b c d v;
        exit 0

let run ast =
  try
    let _, ir = Analyzer.analyze ast in
    Eval.run ir
  with
  | Analyzer.NameError { v; span = sn, (a, b), (c, d) } ->
      Printf.printf "error: %s %d:%d-%d:%d - name %s\n" sn a b c d v;
      exit 0
  | Analyzer.TypeError { v; span = sn, (a, b), (c, d) } ->
      Printf.printf "error: %s %d:%d-%d:%d - type %s\n" sn a b c d v;
      exit 0

let () =
  if Array.length Sys.argv <> 2 then print_endline "usage: mini <filename>"
  else
    let ast, _ = parse_and_import Sys.argv.(1) [] in
    run ast
