open Mini

let analyze ds =
  let ir1 = Typechecker.check ds in
  let _ = Controlchecker.check ir1 in
  let ir2 = Flatten.transform ir1 in
  ir2
