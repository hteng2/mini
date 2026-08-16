open Unix
open Minic_lib
module H = Hashtbl.Make (String)

type hashset = unit H.t

let rec src_to_stream src =
 fun () ->
  match In_channel.input_char src with
  | Some c -> Stream.Head (c, src_to_stream src)
  | None -> Stream.End

let to_real_path path =
  try Unix.realpath path with
  | Unix.Unix_error (Unix.ENOENT, "realpath", path) ->
      Printf.printf "Path '%s' does not exist\n" path;
      exit 0
  | Unix.Unix_error (Unix.EACCES, "realpath", path) ->
      Printf.printf "Permission denied for '%s'\n" path;
      exit 0
  | Unix.Unix_error (err, "realpath", path) ->
      Printf.printf "Error '%s' for '%s'\n" (Unix.error_message err) path;
      exit 0

let rec parse_and_import file (files : hashset) head =
  let file' = to_real_path file in
  if H.mem files file' then raise (Failure "circular import detected")
  else
    try
      let src = open_in file' |> src_to_stream in
      let _ = H.add files file' () in

      let ts = Lexer.stream src file' in
      let ts', imports = Parser.scan_imports ts in

      let dirname = Filename.dirname file' in
      let imports' =
        List.map (fun import -> Filename.concat dirname import) imports
      in

      List.iter
        (fun import ->
          let tail = parse_and_import import files head in
          Queue.add (Ast.Expr tail) head)
        imports';
      let head', tail' = Parser.parse ts' file' in
      Queue.transfer head' head;
      tail'
    with
    | Errors.Expected { v; span = sn, (a, b), (c, d) } ->
        Printf.printf "error: %s %d:%d-%d:%d - expected %s\n" sn a b c d v;
        exit 0
    | Errors.Unexpected { v; span = sn, (a, b), (c, d) } ->
        Printf.printf "error: %s %d:%d-%d:%d - unexpected %s\n" sn a b c d v;
        exit 0

let analyze ast =
  try
    let x = Analyzer.analyze ast in
    x
  with
  | Symresolver.NameError { v; span = sn, (a, b), (c, d) } ->
      Printf.printf "error: %s %d:%d-%d:%d - name %s\n" sn a b c d v;
      exit 0
  | Types.TypeError { v; span = sn, (a, b), (c, d) } ->
      Printf.printf "error: %s %d:%d-%d:%d - type %s\n" sn a b c d v;
      exit 0

let print_usage () =
  print_endline "usage: minic <subcommand>";
  print_endline "  subcommands:";
  print_endline
    "    run   <src>       - performs compiler checks then runs the program";
  print_endline
    "    build <src> <out> - generates a bytecode file runnable with minir"

let time (start, stop, _) name f =
  start name;
  let res = f () in
  stop ();
  res

let () =
  let absolute_path = Filename.concat (Sys.getcwd ()) Sys.argv.(2) in
  match Sys.argv.(1) with
  | "run" ->
      if Array.length Sys.argv <> 3 then (
        print_usage ();
        exit 0);
      let head = Queue.create () in
      let tail = parse_and_import absolute_path (H.create 0) head in
      (head, tail) |> analyze
      (* |> (fun ir ->
        Debug.print_ir ir 0;
        ir) *)
      |> Optimizer.run
      (* |> (fun ir ->
      Debug.print_ir (fst ir) 0;
      ir) *)
      |> Lowering.run
      (* |> (fun (ir3, sc) ->
      Debug.print_ir3 ir3;
      (ir3, sc)) *)
      |> Eval.run
  | "build" ->
      if Array.length Sys.argv <> 4 then (
        print_usage ();
        exit 0);
      let head = Queue.create () in
      let tail = parse_and_import absolute_path (H.create 0) head in
      (head, tail) |> analyze |> Optimizer.run |> Lowering.run
      |> Codegen.run Sys.argv.(3)
  | _ -> print_usage ()
