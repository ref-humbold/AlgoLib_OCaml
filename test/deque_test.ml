(* Tests: Structure of double-ended queue. *)
open OUnit2
open OAssert
open Algolib.Structures.Deque

let numbers = [10; 6; 14; 97; 24; 37; 2; 30; 45; 18; 51; 71; 68; 26]

let rec list_last xs =
  match xs with
  | [x] -> x
  | _ :: xs' -> list_last xs'
  | [] -> failwith "list_last"

(* is_empty_Test_list *)

let is_empty__when_empty__then_true =
  "is_empty__when_empty__then_true" >:: fun _ ->
    (* given *)
    let test_object = empty in
    (* when *)
    let result = is_empty test_object in
    (* then *)
    assert_that result Is.true_

let is_empty__when_not_empty__then_false =
  "is_empty__when_not_empty__then_false" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers in
    (* when *)
    let result = is_empty test_object in
    (* then *)
    assert_that result Is.false_

let is_empty_Test_list =
  test_list [is_empty__when_empty__then_true; is_empty__when_not_empty__then_false]

(* length_Test_list *)

let length__when_empty__then_zero =
  "length__when_empty__then_zero" >:: fun _ ->
    (* given *)
    let test_object = empty in
    (* when *)
    let result = length test_object in
    (* then *)
    assert_that result Is.Int.zero

let length__when_not_empty__then_number_of_elements =
  "length__when_not_empty__then_number_of_elements" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers in
    (* when *)
    let result = length test_object in
    (* then *)
    assert_that result @@ Is.Int.equal_to (List.length numbers)

let length_Test_list =
  test_list [length__when_empty__then_zero; length__when_not_empty__then_number_of_elements]

(* front_Test_list *)

let front__when_empty__then_empty_deque =
  "front__when_empty__then_empty_deque" >:: fun _ ->
    (* given *)
    let test_object = empty in
    (* when *)
    let exec () = front test_object in
    (* then *)
    assert_that exec @@ Is.raising Empty_deque

let front__when_single_element__then_this_element =
  "front__when_single_element__then_this_element" >:: fun _ ->
    (* given *)
    let element = List.hd numbers in
    let test_object = of_seq @@ Seq.return element in
    (* when *)
    let result = front test_object in
    (* then *)
    assert_that result @@ Is.Int.equal_to element

let front__when_multiple_elements__then_first_element =
  "front__when_multiple_elements__then_first_element" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers in
    (* when *)
    let result = front test_object in
    (* then *)
    assert_that result @@ Is.Int.equal_to (List.hd numbers)

let front_Test_list =
  test_list
    [ front__when_empty__then_empty_deque;
      front__when_single_element__then_this_element;
      front__when_multiple_elements__then_first_element ]

(* back_Test_list *)

let back__when_empty__then_empty_deque =
  "back__when_empty__then_empty_deque" >:: fun _ ->
    (* given *)
    let test_object = empty in
    (* when *)
    let exec () = back test_object in
    (* then *)
    assert_that exec @@ Is.raising Empty_deque

let back__when_single_element__then_this_element =
  "back__when_single_element__then_this_element" >:: fun _ ->
    (* given *)
    let element = List.hd numbers in
    let test_object = of_seq @@ Seq.return element in
    (* when *)
    let result = back test_object in
    (* then *)
    assert_that result @@ Is.Int.equal_to element

let back__when_multiple_elements__then_last_element =
  "back__when_multiple_elements__then_last_element" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers in
    (* when *)
    let result = back test_object in
    (* then *)
    assert_that result @@ Is.Int.equal_to (list_last numbers)

let back_Test_list =
  test_list
    [ back__when_empty__then_empty_deque;
      back__when_single_element__then_this_element;
      back__when_multiple_elements__then_last_element ]

(* push_front_Test_list *)

let push_front__when_empty__then_added =
  "push_front__when_empty__then_added" >:: fun _ ->
    (* given *)
    let test_object = empty and element = List.hd numbers in
    (* when *)
    let result = push_front element test_object in
    (* then *)
    assert_that (length result) @@ Is.Int.equal_to 1 ;
    assert_that (front result) @@ Is.Int.equal_to element ;
    assert_that (back result) @@ Is.Int.equal_to element

let push_front__when_new_element__then_added_first =
  "push_front__when_new_element__then_added_first" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers and element = 111 in
    (* when *)
    let result = push_front element test_object in
    (* then *)
    assert_that (length result) @@ Is.Int.equal_to (List.length numbers + 1) ;
    assert_that (front result) @@ Is.Int.equal_to element ;
    assert_that (back result) @@ Is.Int.equal_to (list_last numbers)

let push_front_Test_list =
  test_list [push_front__when_empty__then_added; push_front__when_new_element__then_added_first]

(* back_Test_list *)

let push_back__when_empty__then_added =
  "push_back__when_empty__then_added" >:: fun _ ->
    (* given *)
    let test_object = empty and element = List.hd numbers in
    (* when *)
    let result = push_back element test_object in
    (* then *)
    assert_that (length result) @@ Is.Int.equal_to 1 ;
    assert_that (front result) @@ Is.Int.equal_to element ;
    assert_that (back result) @@ Is.Int.equal_to element

let push_back__when_new_element__then_added_last =
  "push_back__when_new_element__then_added_last" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers and element = 111 in
    (* when *)
    let result = push_back element test_object in
    (* then *)
    assert_that (length result) @@ Is.Int.equal_to (List.length numbers + 1) ;
    assert_that (front result) @@ Is.Int.equal_to (List.hd numbers) ;
    assert_that (back result) @@ Is.Int.equal_to element

let push_back_Test_list =
  test_list [push_back__when_empty__then_added; push_back__when_new_element__then_added_last]

(* pop_front_Test_list *)

let pop_front__when_empty__then_empty_deque =
  "pop_front__when_empty__then_empty_deque" >:: fun _ ->
    (* given *)
    let test_object = empty in
    (* when *)
    let exec () = pop_front test_object in
    (* then *)
    assert_that exec @@ Is.raising Empty_deque

let pop_front__when_single_element__then_this_element_removed =
  "pop_front__when_single_element__then_this_element_removed" >:: fun _ ->
    (* given *)
    let test_object = of_seq @@ Seq.return (List.hd numbers) in
    (* when *)
    let result = pop_front test_object in
    (* then *)
    assert_that (is_empty result) Is.true_

let pop_front__when_multiple_elements__then_first_element_removed =
  "pop_front__when_multiple_elements__then_first_element_removed" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers in
    (* when *)
    let result = pop_front test_object in
    (* then *)
    assert_that (length result) @@ Is.Int.equal_to (List.length numbers - 1) ;
    assert_that (back result) @@ Satisfies.not @@ Is.Int.equal_to (List.hd numbers)

let pop_front_Test_list =
  test_list
    [ pop_front__when_empty__then_empty_deque;
      pop_front__when_single_element__then_this_element_removed;
      pop_front__when_multiple_elements__then_first_element_removed ]

(* pop_back_Test_list *)

let pop_back__when_empty__then_empty_deque =
  "pop_back__when_empty__then_empty_deque" >:: fun _ ->
    (* given *)
    let test_object = empty in
    (* when *)
    let exec () = pop_back test_object in
    (* then *)
    assert_that exec @@ Is.raising Empty_deque

let pop_back__when_single_element__then_this_element_removed =
  "pop_back__when_single_element__then_this_element_removed" >:: fun _ ->
    (* given *)
    let test_object = of_seq @@ Seq.return (List.hd numbers) in
    (* when *)
    let result = pop_back test_object in
    (* then *)
    assert_that (is_empty result) @@ Is.true_

let pop_back__when_multiple_elements__then_last_element_removed =
  "pop_back__when_multiple_elements__then_last_element_removed" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers in
    (* when *)
    let result = pop_back test_object in
    (* then *)
    assert_that (length result) @@ Is.Int.equal_to (List.length numbers - 1) ;
    assert_that (back result) @@ Satisfies.not @@ Is.Int.equal_to (list_last numbers)

let pop_back_Test_list =
  test_list
    [ pop_back__when_empty__then_empty_deque;
      pop_back__when_single_element__then_this_element_removed;
      pop_back__when_multiple_elements__then_last_element_removed ]

(* deque_Test *)

let deque_Test =
  "Tests: Structure of double-ended queue"
  >::: [ is_empty_Test_list;
         length_Test_list;
         front_Test_list;
         back_Test_list;
         push_front_Test_list;
         push_back_Test_list;
         pop_front_Test_list;
         pop_back_Test_list ]

let _ = run_test_tt_main deque_Test
