open! Helpers
open! Syntax
open! Core

let tdce_pipeline () : unit =
  (* Load a Bril program (as JSON) from [stdin] *)
  let json = load_json () in
  let _functions : func list =
    List.map ~f:func_of_json (list_of_json (json $! "functions")) in
  failwith "TODO"
