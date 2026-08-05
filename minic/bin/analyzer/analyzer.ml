open Minic_lib

let analyze ds = ds |> Symresolver.run |> Typechecker.run
