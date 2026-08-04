open Mini

type builtinFn = {
  name : string;
  fnType : Types.t;
  pure : bool;
  def : Values.value array -> Values.value;
}

let builtins : builtinFn list =
  [
    {
      name = "print";
      fnType = Types.Fn (Types.Void, [ Types.Str ]);
      pure = false;
      def =
        (fun s ->
          match s.(0) with
          | Values.Str s ->
              print_string s;
              flush stdout;
              Values.Void
          | _ -> assert false);
    };
    {
      name = "println";
      fnType = Types.Fn (Types.Void, [ Types.Str ]);
      pure = false;
      def =
        (fun s ->
          match s.(0) with
          | Values.Str s ->
              print_endline s;
              flush stdout;
              Values.Void
          | _ -> assert false);
    };
    {
      name = "readline";
      fnType = Types.Fn (Types.Str, []);
      pure = false;
      def = (fun _ -> Values.Str (read_line ()));
    };
    {
      name = "itoa";
      fnType = Types.Fn (Types.Str, [ Types.Int ]);
      pure = true;
      def =
        (fun i ->
          match i.(0) with
          | Values.Int i -> Values.Str (Int.to_string i)
          | _ -> assert false);
    };
    {
      name = "ftoa";
      fnType = Types.Fn (Types.Str, [ Types.Float ]);
      pure = true;
      def =
        (fun i ->
          match i.(0) with
          | Values.Float n -> Values.Str (Float.to_string n)
          | _ -> assert false);
    };
    {
      name = "ftoi";
      fnType = Types.Fn (Types.Int, [ Types.Float ]);
      pure = true;
      def =
        (fun i ->
          match i.(0) with
          | Values.Float n -> Values.Int (Float.to_int n)
          | _ -> assert false);
    };
    {
      name = "itof";
      fnType = Types.Fn (Types.Float, [ Types.Int ]);
      pure = true;
      def =
        (fun i ->
          match i.(0) with
          | Values.Int n -> Values.Float (Float.of_int n)
          | _ -> assert false);
    };
    {
      name = "atoi";
      fnType = Types.Fn (Types.Int, [ Types.Str ]);
      pure = true;
      def =
        (fun s ->
          match s.(0) with
          | Values.Str s -> (
              match int_of_string_opt s with
              | Some i -> Values.Int i
              | None -> Values.Int 0)
          | _ -> assert false);
    };
    {
      name = "rand";
      fnType = Types.Fn (Types.Float, []);
      pure = false;
      def =
        (fun _ ->
          Random.self_init ();
          Values.Float (Random.float 1.0));
    };
  ]
