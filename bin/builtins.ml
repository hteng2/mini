open Mini
module Fns = Hashtbl.Make (String)

type builtinFn = {
  types : Types.t;
  pure : bool;
  def : Values.v list -> Values.v;
}

let builtins : builtinFn Fns.t = Fns.create 0

let () =
  Fns.add builtins "print"
    {
      types = (Types.Fn (Types.Void, [ Types.Str ]), Types.Const);
      pure = false;
      def =
        (fun s ->
          match s with
          | Values.Str s :: [] ->
              print_string s;
              flush stdout;
              Values.Void
          | _ -> assert false);
    };

  Fns.add builtins "println"
    {
      types = (Types.Fn (Types.Void, [ Types.Str ]), Types.Const);
      pure = false;
      def =
        (fun s ->
          match s with
          | Values.Str s :: [] ->
              print_endline s;
              flush stdout;
              Values.Void
          | _ -> assert false);
    };

  Fns.add builtins "readline"
    {
      types = (Types.Fn (Types.Str, []), Types.Const);
      pure = false;
      def = (fun _ -> Values.Str (read_line ()));
    };

  Fns.add builtins "itoa"
    {
      types = (Types.Fn (Types.Str, [ Types.Int ]), Types.Const);
      pure = true;
      def =
        (fun i ->
          match i with
          | Values.Int i :: [] -> Values.Str (Int.to_string i)
          | _ -> assert false);
    }
