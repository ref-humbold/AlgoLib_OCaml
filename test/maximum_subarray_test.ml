(* Tests: Algorithm for maximum_subarray. *)
open OUnit2
open OAssert
open Algolib.Sequences.Maximum_subarray
module IsFloatList = Is.List.Of (Values.Float)
module IsIntList = Is.List.Of (Values.Int)

(* maximum_subarray_Test_list *)

let maximum_subarray__when_all_elements_are_positive__then_whole_sequence =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let sequence = [9.0; 2.4; 3.07; 1.93; 12.67] in
    (* when *)
    let result = maximum_subarray (List.to_seq sequence) in
    (* then *)
    assert_that (List.of_seq result) @@ IsFloatList.equal_to sequence

let maximum_subarray__when_negative_less_than_subsum__then_include_negative =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = maximum_subarray @@ List.to_seq [3.5; 4.8; -1.6; 7.7; 2.1; -9.3; 0.8] in
    (* then *)
    assert_that (List.of_seq result) @@ IsFloatList.equal_to [3.5; 4.8; -1.6; 7.7; 2.1]

let maximum_subarray__when_negative_greater_than_subsum__then_exclude_negative =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = maximum_subarray @@ List.to_seq [-9.3; -1.2; 3.5; 4.8; -10.6; 7.7; 2.1; 0.8; 4.0] in
    (* then *)
    assert_that (List.of_seq result) @@ IsFloatList.equal_to [7.7; 2.1; 0.8; 4.0]

let maximum_subarray__when_all_elements_are_negative__then_empty =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = maximum_subarray @@ List.to_seq [-9.0; -2.4; -3.07; -1.93; -12.67] in
    (* then *)
    assert_that (List.of_seq result) IsFloatList.empty

let maximum_subarray_i__when_negative_less_than_subsum__then_include_negative =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = maximum_subarray_i @@ List.to_seq [3; 5; -2; 7; 2; -9; 1] in
    (* then *)
    assert_that (List.of_seq result) @@ IsIntList.equal_to [3; 5; -2; 7; 2]

let maximum_subarray_i__when_negative_greater_than_subsum__then_exclude_negative =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = maximum_subarray_i @@ List.to_seq [-9; -1; 3; 5; -11; 7; 2; 1; 4] in
    (* then *)
    assert_that (List.of_seq result) @@ IsIntList.equal_to [7; 2; 1; 4]

let maximum_subarray_Test_list =
  test_list
    [ maximum_subarray__when_all_elements_are_positive__then_whole_sequence;
      maximum_subarray__when_negative_less_than_subsum__then_include_negative;
      maximum_subarray__when_negative_greater_than_subsum__then_exclude_negative;
      maximum_subarray__when_all_elements_are_negative__then_empty;
      maximum_subarray_i__when_negative_less_than_subsum__then_include_negative;
      maximum_subarray_i__when_negative_greater_than_subsum__then_exclude_negative ]

(* maximum_subarray_Test *)

let maximum_subarray_Test = __MODULE__ >::: [maximum_subarray_Test_list]

let _ = run_test_tt_main maximum_subarray_Test
