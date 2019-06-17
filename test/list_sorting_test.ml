open OUnit2
open Algolib.List_sorting

(* merge_sort *)

let test_merge_sort =
  "test_merge_sort"
  >:: fun _ ->
    let result = merge_sort compare [3; 17; -6; 0; 9; -12; 7; 4; 2] in
    assert_equal [-12; -6; 0; 2; 3; 4; 7; 9; 17] result

let test_merge_sort_when_all_equal =
  "test_merge_sort_when_all_equal"
  >:: fun _ ->
    let result = merge_sort compare [10; 10; 10; 10; 10; 10; 10; 10; 10] in
    assert_equal [10; 10; 10; 10; 10; 10; 10; 10; 10] result

let test_merge_sort_when_empty_list =
  "test_merge_sort_when_empty_list"
  >:: fun _ ->
    let result = merge_sort compare [] in
    assert_equal [] result

(* list_sorting test *)

let list_sorting_test =
  "list_sorting_test"
  >::: [test_merge_sort; test_merge_sort_when_all_equal; test_merge_sort_when_empty_list]

let _ = run_test_tt_main list_sorting_test
