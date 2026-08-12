open Minic_lib

let run (ir, sc) = ir |> Reducer.run |> fun ir -> (ir, sc)
