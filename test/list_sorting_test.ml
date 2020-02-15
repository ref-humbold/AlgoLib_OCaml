(* Tests: List sorting algorithms *)
open OUnit2
open Algolib.List_sorting

(* merge_sort_Test_list *)

let merge_sort_When_standard_compare_Then_returns_ascending_order =
  "merge_sort When standard compare Then returns ascending order"
  >:: fun _ ->
    let result = merge_sort compare [3; 17; -6; 0; 9; -12; 7; 4; 2] in
    assert_equal ~printer:Printers.int_list [-12; -6; 0; 2; 3; 4; 7; 9; 17] result

let merge_sort_When_reversed_compare_Then_returns_descending_order =
  "merge_sort When reversed compare Then returns descending order"
  >:: fun _ ->
    let result = merge_sort (fun x y -> -compare x y) [3; 17; -6; 0; 9; -12; 7; 4; 2] in
    assert_equal ~printer:Printers.int_list [17; 9; 7; 4; 3; 2; 0; -6; -12] result

let merge_sort_When_all_members_equal_Then_returns_unchanged_list =
  "merge_sort When all members equal Then returns unchanged list"
  >:: fun _ ->
    let result = merge_sort compare [10; 10; 10; 10; 10; 10; 10; 10; 10] in
    assert_equal ~printer:Printers.int_list [10; 10; 10; 10; 10; 10; 10; 10; 10] result

let merge_sort_When_empty_list_Then_returns_empty_list =
  "merge_sort When empty list Then returns empty list"
  >:: fun _ ->
    let result = merge_sort compare [] in
    assert_equal ~printer:Printers.int_list [] result

let merge_sort_Test_list =
  test_list
    [ merge_sort_When_standard_compare_Then_returns_ascending_order;
      merge_sort_When_reversed_compare_Then_returns_descending_order;
      merge_sort_When_all_members_equal_Then_returns_unchanged_list;
      merge_sort_When_empty_list_Then_returns_empty_list ]

(* list_sorting_Test *)

let list_sorting_Test = "Tests: List sorting algorithms" >::: [merge_sort_Test_list]

let _ = run_test_tt_main list_sorting_Test
