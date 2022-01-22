(* Tests: List sorting algorithms *)
open OUnit2
open Algolib.List_sorting
open Test_utils

let numbers = [3; 17; -6; 0; 9; -12; 7; 4; 2]

(* merge_sort_Test_list *)

let merge_sort__when_standard_compare__then_returns_ascending_order =
  "merge_sort When standard compare Then returns ascending order" >:: fun _ ->
    (* when *)
    let result = merge_sort compare numbers in
    (* then *)
    assert_equal ~printer:(Printers.list string_of_int) [-12; -6; 0; 2; 3; 4; 7; 9; 17] result

let merge_sort__when_reversed_compare__then_returns_descending_order =
  "merge_sort When reversed compare Then returns descending order" >:: fun _ ->
    (* when *)
    let result = merge_sort (fun x y -> -compare x y) numbers in
    (* then *)
    assert_equal ~printer:(Printers.list string_of_int) [17; 9; 7; 4; 3; 2; 0; -6; -12] result

let merge_sort__when_all_members_equal__then_returns_unchanged_list =
  "merge_sort When all members equal Then returns unchanged list" >:: fun _ ->
    (* given *)
    let lst = [10; 10; 10; 10; 10; 10; 10; 10; 10] in
    (* when *)
    let result = merge_sort compare lst in
    (* then *)
    assert_equal ~printer:(Printers.list string_of_int) lst result

let merge_sort__when_empty_list__then_returns_empty_list =
  "merge_sort When empty list Then returns empty list" >:: fun _ ->
    (* when *)
    let result = merge_sort compare [] in
    (* then *)
    assert_equal ~printer:(Printers.list string_of_int) [] result

let merge_sort_Test_list =
  test_list
    [ merge_sort__when_standard_compare__then_returns_ascending_order;
      merge_sort__when_reversed_compare__then_returns_descending_order;
      merge_sort__when_all_members_equal__then_returns_unchanged_list;
      merge_sort__when_empty_list__then_returns_empty_list ]

(* list_sorting_Test *)

let list_sorting_Test = "Tests: List sorting algorithms" >::: [merge_sort_Test_list]

let _ = run_test_tt_main list_sorting_Test
