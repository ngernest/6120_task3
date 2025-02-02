open Core
open Helpers
open Syntax

(** Adds a [Nop] after every instruction in a function *)
let add_nops (func : func) : func =
  let new_instrs = List.intersperse func.instrs ~sep:Nop in
  { func with instrs = new_instrs }

(** Pipeline for adding [Nop] instructions after each instruction 
    in every function in a program *)
let add_nops_pipeline () : unit =
  let json = load_json () in
  let functions =
    List.map ~f:func_of_json (list_of_json (json $! "functions")) in
  let updated_functions = List.map ~f:add_nops functions in
  let output_json = json_of_prog updated_functions in
  printf "%s\n" (Yojson.Basic.pretty_to_string output_json)
