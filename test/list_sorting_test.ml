(* Tests: List sorting algorithms. *)
open OUnit2
open OAssert
open Algolib.Sequences.List_sorting

let numbers = [3; 17; -6; 0; 9; -12; 7; 4; 2]

module IsList = Is.List.Of (Values.Int)

(* merge_sort_Test_list *)

let merge_sort__when_standard_compare__then_returns_ascending_order =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = merge_sort compare numbers in
    (* then *)
    assert_that result @@ IsList.equal_to [-12; -6; 0; 2; 3; 4; 7; 9; 17]

let merge_sort__when_reversed_compare__then_returns_descending_order =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = merge_sort (fun x y -> -compare x y) numbers in
    (* then *)
    assert_that result @@ IsList.equal_to [17; 9; 7; 4; 3; 2; 0; -6; -12]

let merge_sort__when_all_members_equal__then_returns_unchanged_list =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let lst = [10; 10; 10; 10; 10; 10; 10; 10; 10] in
    (* when *)
    let result = merge_sort compare lst in
    (* then *)
    assert_that result @@ IsList.equal_to lst

let merge_sort__when_empty_list__then_returns_empty_list =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = merge_sort compare [] in
    (* then *)
    assert_that result IsList.empty

let merge_sort_Test_list =
  test_list
    [ merge_sort__when_standard_compare__then_returns_ascending_order;
      merge_sort__when_reversed_compare__then_returns_descending_order;
      merge_sort__when_all_members_equal__then_returns_unchanged_list;
      merge_sort__when_empty_list__then_returns_empty_list ]

(* list_sorting_Test *)

let list_sorting_Test = __MODULE__ >::: [merge_sort_Test_list]

let _ = run_test_tt_main list_sorting_Test
