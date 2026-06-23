let f x = ()

let parse (src : char Stream.t) (sn : string) : Ast.program option =
  try Lexer.tokenize src sn |> Parser.parse |> fun x -> Some x with
  | Errors.Expected { v; span = sn, (sr, sc), (er, ec) } ->
      Printf.printf "error: %s %d:%d-%d:%d - expected %s\n" sn sr sc er ec v;
      None
  | Errors.Unexpected { v; span = sn, (sr, sc), (er, ec) } ->
      Printf.printf "error: %s %d:%d-%d:%d - unexpected %s\n" sn sr sc er ec v;
      None

let rec src_to_stream src =
  Stream.push (fun () ->
      match In_channel.input_char src with
      | Some c -> Head (c, src_to_stream src)
      | None -> End)

let rec stream_concat ss =
  match ss with
  | [] -> Stream.push (fun () -> Stream.End)
  | s :: ss' ->
      Stream.push (fun () ->
          match Stream.pop s with
          | Stream.Head (x, s') -> Stream.Head (x, stream_concat (s' :: ss'))
          | Stream.End -> Stream.pop (stream_concat ss'))

let () =
  match Array.to_list Sys.argv with
  | [] -> assert false
  | _ :: files -> (
      let ast_opts =
        files
        |> List.map (fun name -> (open_in name |> src_to_stream |> parse) name)
      in
      let asts, has_error =
        ast_opts
        |> List.fold_left
             (fun (u, h) ast_opt ->
               if h then ([], true)
               else
                 match ast_opt with
                 | None -> ([], true)
                 | Some ast -> (ast :: u, false))
             ([], false)
      in
      if has_error then
        try
          let _, ir = asts |> stream_concat |> Analyzer.analyze in
          Eval.run ir
        with Errors.TypeError { v; span = sn, (sr, sc), (er, ec) } ->
          Printf.printf "type error: %s %d:%d-%d:%d %s\n" sn sr sc er ec v)
