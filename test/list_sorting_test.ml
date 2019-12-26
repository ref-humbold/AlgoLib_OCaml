(* Tests: List sorting algorithms. *)
open OUnit2
open Algolib.List_sorting

(* merge_sort *)

let merge_sort_When_standard_compare_Then_returns_ascending_order =
  "Sorting list of arbitrary integers in ascending order with merge sort"
  >:: fun _ ->
    let result = merge_sort compare [3; 17; -6; 0; 9; -12; 7; 4; 2] in
    assert_equal ~printer:Printers.int_list [-12; -6; 0; 2; 3; 4; 7; 9; 17] result

let merge_sort_When_reversed_compare_Then_returns_descending_order =
  "Sorting list of arbitrary integers in descending order with merge sort"
  >:: fun _ ->
    let result = merge_sort (fun x y -> -compare x y) [3; 17; -6; 0; 9; -12; 7; 4; 2] in
    assert_equal ~printer:Printers.int_list [17; 9; 7; 4; 3; 2; 0; -6; -12] result

let merge_sort_When_all_members_equal_Then_returns_unchanged_list =
  "Sorting list of same integers with merge sort"
  >:: fun _ ->
    let result = merge_sort compare [10; 10; 10; 10; 10; 10; 10; 10; 10] in
    assert_equal ~printer:Printers.int_list [10; 10; 10; 10; 10; 10; 10; 10; 10] result

let merge_sort_When_empty_list_Then_returns_empty_list =
  "Sorting empty list with merge sort"
  >:: fun _ ->
    let result = merge_sort compare [] in
    assert_equal ~printer:Printers.int_list [] result

let merge_sort_Test =
  test_list
    [ merge_sort_When_standard_compare_Then_returns_ascending_order;
      merge_sort_When_reversed_compare_Then_returns_descending_order;
      merge_sort_When_all_members_equal_Then_returns_unchanged_list;
      merge_sort_When_empty_list_Then_returns_empty_list ]

(* list_sorting test *)

let list_sorting_Test = "Tests for list sorting algorithms" >::: [merge_sort_Test]

let _ = run_test_tt_main list_sorting_Test
