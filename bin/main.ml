let () =
  let opt =
    try Sys.argv.(1) with Invalid_argument _ -> "tdce+" in
  Lib.Tdce.tdce_pipeline opt
