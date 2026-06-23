let f x = ()

let parse (src : char Stream.t) (sn : string) : Ast.program =
  try Lexer.tokenize src sn |> Parser.parse with
  | Errors.Expected { v; span = sn, (sr, sc), (er, ec) } as e ->
      Printf.printf "error: %s %d:%d-%d:%d - expected %s\n" sn sr sc er ec v;
      raise e
  | Errors.Unexpected { v; span = sn, (sr, sc), (er, ec) } as e ->
      Printf.printf "error: %s %d:%d-%d:%d - unexpected %s\n" sn sr sc er ec v;
      raise e

let rec src_to_stream src =
  Stream.push (fun () ->
      match In_channel.input_char src with
      | Some c -> Head (c, src_to_stream src)
      | None -> End)

let () =
  let files =
    Array.sub Sys.argv 1 (Array.length Sys.argv - 1) |> Array.to_list
  in
  let asts =
    List.map (fun name -> (open_in name |> src_to_stream |> parse) name) files
  in
  try
    let _, ir = List.concat asts |> Analyzer.analyze in
    Eval.eval ir (Closure.empty () :: [])
  with Errors.TypeError { v; span = sn, (sr, sc), (er, ec) } ->
    Printf.printf "type error: %s %d:%d-%d:%d %s\n" sn sr sc er ec v
