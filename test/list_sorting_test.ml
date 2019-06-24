(* Tests: List sorting algorithms. *)
open OUnit2
open Algolib.List_sorting

let string_of_int_list lst =
  "[" ^ List.fold_left (fun acc x -> acc ^ ", " ^ string_of_int x) "" lst ^ "]"

(* merge_sort *)

let merge_sort_when_standard_compare_then_returns_ascending_order =
  "Sorting list of arbitrary integers in ascending order with merge sort"
  >:: fun _ ->
    let result = merge_sort compare [3; 17; -6; 0; 9; -12; 7; 4; 2] in
    assert_equal ~printer:string_of_int_list [-12; -6; 0; 2; 3; 4; 7; 9; 17] result

let merge_sort_when_reversed_compare_then_returns_descending_order =
  "Sorting list of arbitrary integers in descending order with merge sort"
  >:: fun _ ->
    let result = merge_sort (fun x y -> -compare x y) [3; 17; -6; 0; 9; -12; 7; 4; 2] in
    assert_equal ~printer:string_of_int_list [17; 9; 7; 4; 3; 2; 0; -6; -12] result

let merge_sort_when_all_members_equal_then_returns_unchanged_list =
  "Sorting list of same integers with merge sort"
  >:: fun _ ->
    let result = merge_sort compare [10; 10; 10; 10; 10; 10; 10; 10; 10] in
    assert_equal ~printer:string_of_int_list [10; 10; 10; 10; 10; 10; 10; 10; 10] result

let merge_sort_when_empty_list_then_returns_empty_list =
  "Sorting empty list with merge sort"
  >:: fun _ ->
    let result = merge_sort compare [] in
    assert_equal ~printer:string_of_int_list [] result

(* list_sorting test *)

let list_sorting_test =
  "Tests for list sorting algorithms"
  >::: [ merge_sort_when_standard_compare_then_returns_ascending_order;
         merge_sort_when_reversed_compare_then_returns_descending_order;
         merge_sort_when_all_members_equal_then_returns_unchanged_list;
         merge_sort_when_empty_list_then_returns_empty_list ]

let _ = run_test_tt_main list_sorting_test
