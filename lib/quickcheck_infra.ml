open Core
open Base_quickcheck
open Syntax

(* -------------------------------------------------------------------------- *)
(*                            QuickCheck generators                           *)
(* -------------------------------------------------------------------------- *)

(** QuickCheck generator for [label]s: only generates non-empty 
    alphanumeric strings *)
let quickcheck_generator_label : label Generator.t =
  Generator.(string_non_empty_of char_alphanum)

(** QuickCheck generator for [arg]s: only generates non-empty 
        alphanumeric strings *)
let quick_generator_arg : arg Generator.t =
  Generator.(string_non_empty_of char_alphanum)

(** QuickCheck generator for [literal]s: 
    - Generates small positives [int]s with probability 0.8
    - Generates [bool]s the remaining time *)
let quickcheck_generator_literal : literal Generator.t =
  let open Generator in
  let open Let_syntax in
  weighted_union
    [
      (0.8, small_strictly_positive_int >>| fun i -> LitInt i);
      (0.2, [%quickcheck.generator: bool] >>| fun b -> LitBool b);
    ]

(** QuickCheck generator for destination variables *)
let quickcheck_generator_dest : dest Generator.t =
  Generator.(
    both (string_non_empty_of char_alphanum) [%quickcheck.generator: ty])

(* -------------------------------------------------------------------------- *)
(*                          Round-trip serialization properties               *)
(* -------------------------------------------------------------------------- *)

let%quick_test "round-trip property for function serialization" =
 fun (func : func) ->
  let func' = func_of_json (json_of_func func) in
  let result = equal_func func func' in
  assert result;
  [%expect {| |}]

let%quick_test "round-trip property for instruction serialization" =
 fun (instr : instr) ->
  let instr' = instr_of_json (json_of_instr instr) in
  let result = equal_instr instr instr' in
  assert result;
  [%expect {| |}]

let%quick_test "round-trip property for binop serialization" =
 fun (binop : binop) ->
  let result = binop_of_string (string_of_binop binop) in
  assert (equal_binop binop result);
  [%expect {| |}]

let%quick_test "round-trip property for unop serialization" =
 fun (unop : unop) ->
  let result = unop_of_string (string_of_unop unop) in
  assert (equal_unop unop result);
  [%expect {| |}]
