(* Tests: Structure of binomial heap *)
open OUnit2
open Algolib.Binomial_heap
open TestUtils

let numbers = [10; 6; 14; 97; 24; 37; 2; 30; 45; 18; 51; 71; 68; 26]

let minimum = List.fold_left (fun acc e -> min acc e) max_int numbers

module IntHeap = Make (struct
    type t = int

    let compare = Stdlib.compare
  end)

let rec to_list heap =
  if IntHeap.is_empty heap then [] else IntHeap.peek heap :: to_list (IntHeap.pop heap)

(* is_empty_Test_list *)

let is_empty__when_empty__then_true =
  "is_empty__when_empty__then_true" >:: fun _ ->
    (* given *)
    let test_object = IntHeap.empty in
    (* when *)
    let result = IntHeap.is_empty test_object in
    (* then *)
    Assert.Bool.assert_true result

let is_empty__when_not_empty__then_false =
  "is_empty__when_not_empty__then_false" >:: fun _ ->
    (* given *)
    let test_object = IntHeap.of_list numbers in
    (* when *)
    let result = IntHeap.is_empty test_object in
    (* then *)
    Assert.Bool.assert_false result

let is_empty_Test_list =
  test_list [is_empty__when_empty__then_true; is_empty__when_not_empty__then_false]

(* length_Test_list *)

let length__when_empty__then_zero =
  "length__when_empty__then_zero" >:: fun _ ->
    (* given *)
    let test_object = IntHeap.empty in
    (* when *)
    let result = IntHeap.length test_object in
    (* then *)
    assert_equal ~printer:string_of_int 0 result

let length__when_not_empty__then_number_of_elements =
  "length__when_not_empty__then_number_of_elements" >:: fun _ ->
    (* given *)
    let test_object = IntHeap.of_list numbers in
    (* when *)
    let result = IntHeap.length test_object in
    (* then *)
    assert_equal ~printer:string_of_int (List.length numbers) result

let length_Test_list =
  test_list [length__when_empty__then_zero; length__when_not_empty__then_number_of_elements]

(* peek_Test_list *)

let peek__when_empty__then_empty_heap =
  "peek__when_empty__then_empty_heap" >:: fun _ ->
    (* given *)
    let test_object = IntHeap.empty in
    (* when *)
    let exec () = IntHeap.peek test_object in
    (* then *)
    assert_raises IntHeap.Empty_heap exec

let peek__when_single_element__then_this_element =
  "peek__when_single_element__then_this_element" >:: fun _ ->
    (* given *)
    let element = List.hd numbers in
    let test_object = IntHeap.of_seq @@ Seq.return element in
    (* when *)
    let result = IntHeap.peek test_object in
    (* then *)
    assert_equal ~printer:string_of_int element result

let peek__when_multiple_elements__then_minimal_element =
  "peek__when_multiple_elements__then_minimal_element" >:: fun _ ->
    (* given *)
    let test_object = IntHeap.of_list numbers in
    (* when *)
    let result = IntHeap.peek test_object in
    (* then *)
    assert_equal ~printer:string_of_int minimum result

let peek_opt__when_empty__then_none =
  "peek_opt__when_empty__then_none" >:: fun _ ->
    (* given *)
    let test_object = IntHeap.empty in
    (* when *)
    let result = IntHeap.peek_opt test_object in
    (* then *)
    Assert.Option.assert_none ~printer:string_of_int result

let peek_opt__when_multiple_elements__then_minimal_element =
  "peek_opt__when_multiple_elements__then_minimal_element" >:: fun _ ->
    (* given *)
    let test_object = IntHeap.of_list numbers in
    (* when *)
    let result = IntHeap.peek_opt test_object in
    (* then *)
    Assert.Option.assert_some ~printer:string_of_int minimum result

let peek_Test_list =
  test_list
    [ peek__when_empty__then_empty_heap;
      peek__when_single_element__then_this_element;
      peek__when_multiple_elements__then_minimal_element;
      peek_opt__when_empty__then_none;
      peek_opt__when_multiple_elements__then_minimal_element ]

(* push_Test_list *)

let push__when_empty__then_added =
  "push__when_empty__then_added" >:: fun _ ->
    (* given *)
    let element = List.hd numbers in
    let test_object = IntHeap.empty in
    (* when *)
    let result = IntHeap.push element test_object in
    (* then *)
    assert_equal ~printer:string_of_int 1 @@ IntHeap.length result ;
    assert_equal ~printer:string_of_int element (IntHeap.peek result)

let push__when_new_element__then_added =
  "push__when_new_element__then_added" >:: fun _ ->
    (* given *)
    let test_object = IntHeap.of_list numbers in
    (* when *)
    let result = IntHeap.push 111 test_object in
    (* then *)
    assert_equal ~printer:string_of_int (List.length numbers + 1) @@ IntHeap.length result ;
    assert_equal ~printer:string_of_int minimum (IntHeap.peek result)

let push__when_new_element_is_greater_than_minimum__then_new_minimum =
  "push__when_new_element_is_greater_than_minimum__then_new_minimum" >:: fun _ ->
    (* given *)
    let test_object = IntHeap.of_list numbers and element = minimum - 3 in
    (* when *)
    let result = IntHeap.push element test_object in
    (* then *)
    assert_equal ~printer:string_of_int (List.length numbers + 1) @@ IntHeap.length result ;
    assert_equal ~printer:string_of_int element (IntHeap.peek result)

let push_Test_list =
  test_list
    [ push__when_empty__then_added;
      push__when_new_element__then_added;
      push__when_new_element_is_greater_than_minimum__then_new_minimum ]

(* pop_Test_list *)

let pop__when_empty__then_empty_heap =
  "pop__when_empty__then_empty_heap" >:: fun _ ->
    (* given *)
    let test_object = IntHeap.empty in
    (* when *)
    let exec () = IntHeap.pop test_object in
    (* then *)
    assert_raises IntHeap.Empty_heap exec

let pop__when_single_element__then_this_element_removed =
  "pop__when_single_element__then_this_element_removed" >:: fun _ ->
    (* given *)
    let test_object = IntHeap.of_seq @@ Seq.return (List.hd numbers) in
    (* when *)
    let result = IntHeap.pop test_object in
    (* then *)
    assert_equal ~printer:string_of_int 0 @@ IntHeap.length result ;
    Assert.Bool.assert_true @@ IntHeap.is_empty result

let pop__when_multiple_elements__then_minimal_element_removed =
  "pop__when_multiple_elements__then_minimal_element_removed" >:: fun _ ->
    (* given *)
    let test_object = IntHeap.of_list numbers in
    (* when *)
    let result = IntHeap.pop test_object in
    (* then *)
    assert_equal ~printer:string_of_int (List.length numbers - 1) @@ IntHeap.length result ;
    Assert.assert_not_equal ~printer:string_of_int minimum (IntHeap.peek result)

let pop__when_multiple_calls__then_sorted_ascending =
  "pop__when_multiple_calls__then_sorted_ascending" >:: fun _ ->
    (* given *)
    let test_object = IntHeap.of_list numbers and expected = List.sort compare numbers in
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
  "merge__when_empty_and_not_empty__then_same_as_other" >:: fun _ ->
    (* given *)
    let test_object = IntHeap.empty and other = IntHeap.of_list numbers in
    (* when *)
    let result = IntHeap.merge test_object other in
    (* then *)
    assert_equal ~printer:string_of_int (IntHeap.length other) @@ IntHeap.length result ;
    assert_equal ~printer:string_of_int (IntHeap.peek other) @@ IntHeap.peek result

let merge__when_not_empty_and_empty__then_no_changes =
  "merge__when_not_empty_and_empty__then_no_changes" >:: fun _ ->
    (* given *)
    let test_object = IntHeap.of_list numbers in
    (* when *)
    let result = IntHeap.merge test_object IntHeap.empty in
    (* then *)
    assert_equal ~printer:string_of_int (List.length numbers) @@ IntHeap.length result ;
    assert_equal ~printer:string_of_int minimum @@ IntHeap.peek result

let merge__when_other_has_greater_minimum__then_new_minimum =
  "merge__when_other_has_greater_minimum__then_new_minimum" >:: fun _ ->
    (* given *)
    let new_minimum = minimum - 4 in
    let test_object = IntHeap.of_list numbers
    and other = IntHeap.of_list [new_minimum; minimum + 5; minimum + 13; minimum + 20] in
    (* when *)
    let result = IntHeap.merge test_object other in
    (* then *)
    assert_equal ~printer:string_of_int (List.length numbers + IntHeap.length other)
    @@ IntHeap.length result ;
    assert_equal ~printer:string_of_int new_minimum @@ IntHeap.peek result

let merge__when_other_has_less_minimum__then_minimum_remains =
  "merge__when_other_has_less_minimum__then_minimum_remains" >:: fun _ ->
    (* given *)
    let test_object = IntHeap.of_list numbers
    and other = IntHeap.of_list [minimum + 5; minimum + 13; minimum + 20] in
    (* when *)
    let result = IntHeap.merge test_object other in
    (* then *)
    assert_equal ~printer:string_of_int (List.length numbers + IntHeap.length other)
    @@ IntHeap.length result ;
    assert_equal ~printer:string_of_int minimum @@ IntHeap.peek result

let merge__when_shared_inner_heap__then_changed_only_merging_heap =
  "merge__when_shared_inner_heap__then_changed_only_merging_heap" >:: fun _ ->
    (* given *)
    let test_object = IntHeap.empty
    and first = IntHeap.of_list [10; 20]
    and second = IntHeap.of_list [4; 8] in
    (* when *)
    let result1 = IntHeap.merge test_object first in
    let result2 = IntHeap.merge result1 second in
    (* then *)
    assert_equal ~printer:string_of_int 10 @@ IntHeap.peek result1 ;
    assert_equal ~printer:(Printers.list string_of_int) (to_list result1) [10; 20] ;
    assert_equal ~printer:string_of_int 4 (IntHeap.peek result2) ;
    assert_equal ~printer:(Printers.list string_of_int) (to_list result2) [4; 8; 10; 20] ;
    Assert.Bool.assert_true @@ IntHeap.is_empty test_object ;
    assert_equal ~printer:(Printers.list string_of_int) (to_list first) [10; 20] ;
    assert_equal ~printer:(Printers.list string_of_int) (to_list second) [4; 8]

let merge_Test_list =
  test_list
    [ merge__when_empty_and_not_empty__then_same_as_other;
      merge__when_not_empty_and_empty__then_no_changes;
      merge__when_other_has_greater_minimum__then_new_minimum;
      merge__when_other_has_less_minimum__then_minimum_remains;
      merge__when_shared_inner_heap__then_changed_only_merging_heap ]

(* binomial_heap_Test *)

let binomial_heap_Test =
  "Tests: Structure of binomial heap"
  >::: [ is_empty_Test_list;
         length_Test_list;
         peek_Test_list;
         push_Test_list;
         pop_Test_list;
         merge_Test_list ]

let _ = run_test_tt_main binomial_heap_Test
