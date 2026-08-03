open Mini

let analyze ds = ds |> Typechecker.run |> Reducer.run
