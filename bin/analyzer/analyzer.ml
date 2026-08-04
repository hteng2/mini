open Mini

let analyze ds = ds |> Symresolver.run |> Typechecker.run
