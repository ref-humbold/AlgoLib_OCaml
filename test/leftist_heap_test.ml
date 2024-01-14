(* Tests: Structure of leftist heap. *)
open OUnit2
open Algolib.Leftist_heap
open TestUtils

let numbers = [10; 6; 14; 97; 24; 37; 2; 30; 45; 18; 51; 71; 68; 26]

let minimum = List.fold_left (fun acc e -> min acc e) max_int numbers

module IntLH = Make (struct
    type t = int

    let compare = Stdlib.compare
  end)

let rec to_list heap =
  if IntLH.is_empty heap then [] else IntLH.peek heap :: to_list (IntLH.pop heap)

(* is_empty_Test_list *)

let is_empty__when_empty__then_true =
  "is_empty When empty Then true" >:: fun _ ->
    (* given *)
    let test_object = IntLH.empty in
    (* when *)
    let result = IntLH.is_empty test_object in
    (* then *)
    Assert.Bool.assert_true result

let is_empty__when_not_empty__then_false =
  "is_empty When not empty Then false" >:: fun _ ->
    (* given *)
    let test_object = IntLH.of_list numbers in
    (* when *)
    let result = IntLH.is_empty test_object in
    (* then *)
    Assert.Bool.assert_false result

let is_empty_Test_list =
  test_list [is_empty__when_empty__then_true; is_empty__when_not_empty__then_false]

(* peek_Test_list *)

let peek__when_empty__then_empty_heap =
  "peek When empty Then empty heap" >:: fun _ ->
    (* given *)
    let test_object = IntLH.empty in
    (* when *)
    let exec () = IntLH.peek test_object in
    (* then *)
    assert_raises IntLH.Empty_heap exec

let peek__when_single_element__then_this_element =
  "peek When single element Then this element" >:: fun _ ->
    (* given *)
    let element = 19 in
    let test_object = IntLH.of_seq @@ Seq.return element in
    (* when *)
    let result = IntLH.peek test_object in
    (* then *)
    assert_equal ~printer:string_of_int element result

let peek__when_multiple_elements__then_minimal_element =
  "peek When multiple elements Then minimal element" >:: fun _ ->
    (* given *)
    let test_object = IntLH.of_list numbers in
    (* when *)
    let result = IntLH.peek test_object in
    (* then *)
    assert_equal ~printer:string_of_int minimum result

let peek_Test_list =
  test_list
    [ peek__when_empty__then_empty_heap;
      peek__when_single_element__then_this_element;
      peek__when_multiple_elements__then_minimal_element ]

(* push_Test_list *)

let push__when_empty__then_added =
  "push When empty Then added" >:: fun _ ->
    (* given *)
    let element = 19 in
    let test_object = IntLH.empty in
    (* when *)
    let result = IntLH.push element test_object in
    (* then *)
    assert_equal ~printer:string_of_int element (IntLH.peek result)

let push__when_new_element__then_added =
  "push When new element Then added" >:: fun _ ->
    (* given *)
    let test_object = IntLH.of_list numbers in
    (* when *)
    let result = IntLH.push 46 test_object in
    (* then *)
    assert_equal ~printer:string_of_int minimum (IntLH.peek result)

let push__when_new_element_is_greater_than_minimum__then_new_minimum =
  "push When new element is greater than minimum Then new minimum" >:: fun _ ->
    (* given *)
    let test_object = IntLH.of_list numbers and element = minimum - 3 in
    (* when *)
    let result = IntLH.push element test_object in
    (* then *)
    assert_equal ~printer:string_of_int element (IntLH.peek result)

let push_Test_list =
  test_list
    [ push__when_empty__then_added;
      push__when_new_element__then_added;
      push__when_new_element_is_greater_than_minimum__then_new_minimum ]

(* pop_Test_list *)

let pop__when_empty__then_empty_heap =
  "pop When empty Then empty heap" >:: fun _ ->
    (* given *)
    let test_object = IntLH.empty in
    (* when *)
    let exec () = IntLH.pop test_object in
    (* then *)
    assert_raises IntLH.Empty_heap exec

let pop__when_single_element__then_this_element_removed =
  "pop When single element Then this element removed" >:: fun _ ->
    (* given *)
    let test_object = IntLH.of_seq @@ Seq.return 19 in
    (* when *)
    let result = IntLH.pop test_object in
    (* then *)
    Assert.Bool.assert_true @@ IntLH.is_empty result

let pop__when_multiple_elements__then_minimal_element_removed =
  "pop When multiple elements Then minimal element removed" >:: fun _ ->
    (* given *)
    let test_object = IntLH.of_list numbers in
    (* when *)
    let result = IntLH.pop test_object in
    (* then *)
    Assert.assert_not_equal ~printer:string_of_int minimum (IntLH.peek result)

let pop__when_multiple_calls__then_sorted_ascending =
  "pop When multiple calls Then sorted ascending" >:: fun _ ->
    (* given *)
    let test_object = IntLH.of_list numbers and expected = List.sort compare numbers in
    (* when *)
    let result = to_list test_object in
    (* then *)
    assert_equal ~printer:(Printers.list string_of_int) expected result

let pop_Test_list =
  test_list
    [ pop__when_empty__then_empty_heap;
      pop__when_single_element__then_this_element_removed;
      pop__when_multiple_elements__then_minimal_element_removed;
      pop__when_multiple_calls__then_sorted_ascending ]

(* merge_Test_list *)

let merge__when_empty_and_not_empty__then_same_as_other =
  "merge When empty and not empty Then same as other" >:: fun _ ->
    (* given *)
    let test_object = IntLH.empty and other = IntLH.of_list numbers in
    (* when *)
    let result = IntLH.merge test_object other in
    (* then *)
    assert_equal ~printer:string_of_int (List.length @@ to_list other) (List.length @@ to_list result) ;
    assert_equal ~printer:string_of_int (IntLH.peek other) (IntLH.peek result)

let merge__when_not_empty_and_empty__then_no_changes =
  "merge When not empty and empty Then no changes" >:: fun _ ->
    (* given *)
    let test_object = IntLH.of_list numbers in
    (* when *)
    let result = IntLH.merge test_object IntLH.empty in
    (* then *)
    assert_equal ~printer:string_of_int (List.length numbers) (List.length @@ to_list result) ;
    assert_equal ~printer:string_of_int minimum (IntLH.peek result)

let merge__when_other_has_greater_minimum__then_new_minimum =
  "merge When other has greater minimum Then new minimum" >:: fun _ ->
    (* given *)
    let new_minimum = minimum - 4 in
    let test_object = IntLH.of_list numbers
    and other = IntLH.of_list [new_minimum; minimum + 5; minimum + 13; minimum + 20] in
    (* when *)
    let result = IntLH.merge test_object other in
    (* then *)
    assert_equal
      ~printer:string_of_int
      (List.length numbers + (List.length @@ to_list other))
      (List.length @@ to_list result) ;
    assert_equal ~printer:string_of_int new_minimum (IntLH.peek result)

let merge__when_other_has_less_minimum__then_minimum_remains =
  "merge When other has less minimum Then minimum remains" >:: fun _ ->
    (* given *)
    let test_object = IntLH.of_list numbers
    and other = IntLH.of_list [minimum + 5; minimum + 13; minimum + 20] in
    (* when *)
    let result = IntLH.merge test_object other in
    (* then *)
    assert_equal
      ~printer:string_of_int
      (List.length numbers + (List.length @@ to_list other))
      (List.length @@ to_list result) ;
    assert_equal ~printer:string_of_int minimum (IntLH.peek result)

let merge__when_shared_inner_heap__then_changed_only_merging_heap =
  "merge When shared inner heap Then changed only merging heap" >:: fun _ ->
    (* given *)
    let test_object = IntLH.empty
    and first = IntLH.of_list [10; 20]
    and second = IntLH.of_list [4; 8] in
    (* when *)
    let result1 = IntLH.merge test_object first in
    let result2 = IntLH.merge result1 second in
    (* then *)
    assert_equal ~printer:string_of_int 10 (IntLH.peek result1) ;
    assert_equal ~printer:(Printers.list string_of_int) (to_list result1) [10; 20] ;
    assert_equal ~printer:string_of_int 4 (IntLH.peek result2) ;
    assert_equal ~printer:(Printers.list string_of_int) (to_list result2) [4; 8; 10; 20] ;
    Assert.Bool.assert_true @@ IntLH.is_empty test_object ;
    assert_equal ~printer:(Printers.list string_of_int) (to_list first) [10; 20] ;
    assert_equal ~printer:(Printers.list string_of_int) (to_list second) [4; 8]

let merge_Test_list =
  test_list
    [ merge__when_empty_and_not_empty__then_same_as_other;
      merge__when_not_empty_and_empty__then_no_changes;
      merge__when_other_has_greater_minimum__then_new_minimum;
      merge__when_other_has_less_minimum__then_minimum_remains;
      merge__when_shared_inner_heap__then_changed_only_merging_heap ]

(* leftist_heap_Test *)

let leftist_heap_Test =
  "Tests: Structure of leftist heap"
  >::: [is_empty_Test_list; peek_Test_list; push_Test_list; pop_Test_list; merge_Test_list]

let _ = run_test_tt_main leftist_heap_Test
