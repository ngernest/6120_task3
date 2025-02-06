open Core
open Lib.Helpers
open Lib.Syntax

let () =
  let argv = Sys.get_argv () in
  let opt = try argv.(1) with Invalid_argument _ -> "full" in

  let json = load_json () in
  (* Convert the JSON to our typed representation *)
  let functions =
    List.map ~f:func_of_json (list_of_json (json $! "functions"))
  in

  let opt_fun =
    match opt with
    | "tdce" -> Lib.Tdce.trivial_dce
    | "dkp" -> Lib.Tdce.drop_killed
    | "tdce+" -> Lib.Tdce.tdce_plus
    | "lvn" -> Lib.Lvn.lvn
    | _ -> Lib.Full.full
  in

  let updated_prog = List.map ~f:opt_fun functions in
  (* Convert the optimization program to JSON & write to stdout *)
  Yojson.Safe.pretty_to_channel stdout (json_of_prog updated_prog)
