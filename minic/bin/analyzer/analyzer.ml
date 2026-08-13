open Minic_lib

let analyze ds =
  ds |> Symresolver.run
  (* |> (fun ir1 ->
  Debug.print_ir1 (fst ir1) 0;
  ir1) *)
  |> Typechecker.run
