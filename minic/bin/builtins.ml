open Minic_lib

type builtinType = { name : string; t : Types.t }

let ts : builtinType list =
  [
    { name = "int"; t = Types.Int };
    { name = "float"; t = Types.Float };
    { name = "bool"; t = Types.Bool };
    { name = "char"; t = Types.Char };
    { name = "void"; t = Types.Void };
  ]

type builtinFn = {
  name : string;
  fnType : Types.t;
  pure : bool;
  def :
    (int -> Values.value array -> Values.value) ->
    Values.value Stack.t ->
    Values.value;
}

let fns : builtinFn list =
  [
    {
      name = "print";
      fnType = Types.Fn (Types.List Types.Char, Types.Void);
      pure = false;
      def =
        (fun _ s ->
          match Stack.pop s with
          | Values.List s ->
              Array.iter
                (fun c ->
                  match c with
                  | Values.Char c -> print_char c
                  | _ -> assert false)
                s;
              flush stdout;
              Values.Void
          | _ -> assert false);
    };
    {
      name = "println";
      fnType = Types.Fn (Types.List Types.Char, Types.Void);
      pure = false;
      def =
        (fun _ s ->
          match Stack.pop s with
          | Values.List s ->
              Array.iter
                (fun c ->
                  match c with
                  | Values.Char c -> print_char c
                  | _ -> assert false)
                s;
              print_newline ();
              flush stdout;
              Values.Void
          | _ -> assert false);
    };
    {
      name = "readline";
      fnType = Types.Fn (Types.Void, Types.List Types.Char);
      pure = false;
      def =
        (fun _ _ ->
          Values.List
            (Array.of_list
               (read_line () |> String.to_seq |> List.of_seq
               |> List.map (fun c -> Values.Char c))));
    };
    {
      name = "itoa";
      fnType = Types.Fn (Types.Int, Types.List Types.Char);
      pure = true;
      def =
        (fun _ i ->
          match Stack.pop i with
          | Values.Int i ->
              let str =
                Int.to_string i |> String.to_seq |> List.of_seq
                |> List.map (fun c -> Values.Char c)
                |> Array.of_list
              in
              Values.List str
          | _ -> assert false);
    };
    {
      name = "ftoa";
      fnType = Types.Fn (Types.Float, Types.List Types.Char);
      pure = true;
      def =
        (fun _ i ->
          match Stack.pop i with
          | Values.Float n ->
              let str =
                Float.to_string n |> String.to_seq |> List.of_seq
                |> List.map (fun c -> Values.Char c)
                |> Array.of_list
              in
              Values.List str
          | _ -> assert false);
    };
    {
      name = "ftoi";
      fnType = Types.Fn (Types.Float, Types.Int);
      pure = true;
      def =
        (fun _ i ->
          match Stack.pop i with
          | Values.Float n -> Values.Int (Float.to_int n)
          | _ -> assert false);
    };
    {
      name = "itof";
      fnType = Types.Fn (Types.Int, Types.Float);
      pure = true;
      def =
        (fun _ i ->
          match Stack.pop i with
          | Values.Int n -> Values.Float (Float.of_int n)
          | _ -> assert false);
    };
    {
      name = "atoi";
      fnType = Types.Fn (Types.List Types.Char, Types.Int);
      pure = true;
      def =
        (fun _ s ->
          match Stack.pop s with
          | Values.List s -> (
              match
                s
                |> Array.map (fun v ->
                    match v with Values.Char c -> c | _ -> assert false)
                |> Array.to_seq |> String.of_seq |> int_of_string_opt
              with
              | Some i -> Values.Int i
              | None -> Values.Int 0)
          | _ -> assert false);
    };
    {
      name = "rand";
      fnType = Types.Fn (Types.Void, Types.Float);
      pure = false;
      def =
        (fun _ _ ->
          Random.self_init ();
          Values.Float (Random.float 1.0));
    };
    {
      name = "list";
      fnType =
        Types.Fn
          ( Types.Tuple [ Types.Fn (Types.Int, Types.All (-1)); Types.Int ],
            Types.List (Types.All (-1)) );
      pure = false;
      def =
        (fun h v ->
          match Stack.pop v with
          | Values.Tuple [ fn; len ] -> (
              match (fn, len) with
              | Values.Fn (closure, symcnt, loc), Values.Int n ->
                  let vs =
                    Array.init n (fun i ->
                        let scope' = Array.make symcnt Values.Nil in
                        scope'.(0) <- fn;
                        Array.iteri (fun i v -> scope'.(i + 1) <- v) closure;
                        Stack.push (Values.Int i) v;
                        h loc scope')
                  in
                  Values.List vs
              | _ -> assert false)
          | _ -> assert false);
    };
  ]
