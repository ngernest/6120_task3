open Core
open Base_quickcheck
open Generator
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
      (0.8, int64 >>| fun i -> LitInt i);
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

(* -------------------------------------------------------------------------- *)
(*                       Properties for helper functions                      *)
(* -------------------------------------------------------------------------- *)

let%expect_test "unit test for remove_keys" =
  (* Suppress stack trace in order to make expect tests reproducible *)
  Printexc.record_backtrace false;
  let lhs =
    Helpers.remove_keys [ 1; 2; 3 ] ~equal:Int.equal
      [ (1, 'a'); (5, 'e'); (2, 'b'); (4, 'd'); (3, 'c') ] in
  let rhs = [ (5, 'e'); (4, 'd') ] in
  assert (List.equal [%equal: int * char] lhs rhs);
  [%expect {| |}]

let%quick_test "[remove_keys] results in association lists that are the same \
                length or shorter" =
 fun (keys : (int list[@generator list small_strictly_positive_int]))
   (assoc_list :
     ((int * char) list
     [@generator list (both small_strictly_positive_int char)])) ->
  let result = Helpers.remove_keys keys ~equal:Int.equal assoc_list in
  assert (List.length result <= List.length assoc_list);
  [%expect {| |}]

let%quick_test "the keys in the resultant association list produced by \
                [remove_keys] are a subset of the original assoc list's keys" =
 fun (keys : (int list[@generator list small_strictly_positive_int]))
   (assoc_list :
     ((int * char) list
     [@generator list (both small_strictly_positive_int char)])) ->
  let original_keys = List.map ~f:fst assoc_list in
  let resultant_keys =
    List.map ~f:fst (Helpers.remove_keys keys ~equal:Int.equal assoc_list) in
  assert (
    List.for_all resultant_keys ~f:(fun k ->
        List.mem original_keys k ~equal:Int.equal));
  [%expect {| |}]
