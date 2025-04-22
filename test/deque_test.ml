(* Tests: Structure of double-ended queue. *)
open OUnit2
open Algolib.Structures.Deque
open TestUtils

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
    Assert.Bool.assert_true result

let is_empty__when_not_empty__then_false =
  "is_empty__when_not_empty__then_false" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers in
    (* when *)
    let result = is_empty test_object in
    (* then *)
    Assert.Bool.assert_false result

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
    assert_equal ~printer:string_of_int 0 result

let length__when_not_empty__then_number_of_elements =
  "length__when_not_empty__then_number_of_elements" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers in
    (* when *)
    let result = length test_object in
    (* then *)
    assert_equal ~printer:string_of_int (List.length numbers) result

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
    assert_raises Empty_deque exec

let front__when_single_element__then_this_element =
  "front__when_single_element__then_this_element" >:: fun _ ->
    (* given *)
    let element = List.hd numbers in
    let test_object = of_seq @@ Seq.return element in
    (* when *)
    let result = front test_object in
    (* then *)
    assert_equal ~printer:string_of_int element result

let front__when_multiple_elements__then_first_element =
  "front__when_multiple_elements__then_first_element" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers in
    (* when *)
    let result = front test_object in
    (* then *)
    assert_equal ~printer:string_of_int (List.hd numbers) result

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
    assert_raises Empty_deque exec

let back__when_single_element__then_this_element =
  "back__when_single_element__then_this_element" >:: fun _ ->
    (* given *)
    let element = List.hd numbers in
    let test_object = of_seq @@ Seq.return element in
    (* when *)
    let result = back test_object in
    (* then *)
    assert_equal ~printer:string_of_int element result

let back__when_multiple_elements__then_last_element =
  "back__when_multiple_elements__then_last_element" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers in
    (* when *)
    let result = back test_object in
    (* then *)
    assert_equal ~printer:string_of_int (list_last numbers) result

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
    assert_equal ~printer:string_of_int 1 @@ length result ;
    assert_equal ~printer:string_of_int element @@ front result ;
    assert_equal ~printer:string_of_int element @@ back result

let push_front__when_new_element__then_added_first =
  "push_front__when_new_element__then_added_first" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers and element = 111 in
    (* when *)
    let result = push_front element test_object in
    (* then *)
    assert_equal ~printer:string_of_int (List.length numbers + 1) @@ length result ;
    assert_equal ~printer:string_of_int element @@ front result ;
    assert_equal ~printer:string_of_int (list_last numbers) @@ back result

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
    assert_equal ~printer:string_of_int 1 @@ length result ;
    assert_equal ~printer:string_of_int element @@ front result ;
    assert_equal ~printer:string_of_int element @@ back result

let push_back__when_new_element__then_added_last =
  "push_back__when_new_element__then_added_last" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers and element = 111 in
    (* when *)
    let result = push_back element test_object in
    (* then *)
    assert_equal ~printer:string_of_int (List.length numbers + 1) @@ length result ;
    assert_equal ~printer:string_of_int (List.hd numbers) @@ front result ;
    assert_equal ~printer:string_of_int element @@ back result

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
    assert_raises Empty_deque exec

let pop_front__when_single_element__then_this_element_removed =
  "pop_front__when_single_element__then_this_element_removed" >:: fun _ ->
    (* given *)
    let test_object = of_seq @@ Seq.return (List.hd numbers) in
    (* when *)
    let result = pop_front test_object in
    (* then *)
    Assert.Bool.assert_true @@ is_empty result

let pop_front__when_multiple_elements__then_first_element_removed =
  "pop_front__when_multiple_elements__then_first_element_removed" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers in
    (* when *)
    let result = pop_front test_object in
    (* then *)
    assert_equal ~printer:string_of_int (List.length numbers - 1) @@ length result ;
    Assert.assert_not_equal ~printer:string_of_int (back result) (List.hd numbers)

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
    assert_raises Empty_deque exec

let pop_back__when_single_element__then_this_element_removed =
  "pop_back__when_single_element__then_this_element_removed" >:: fun _ ->
    (* given *)
    let test_object = of_seq @@ Seq.return (List.hd numbers) in
    (* when *)
    let result = pop_back test_object in
    (* then *)
    Assert.Bool.assert_true @@ is_empty result

let pop_back__when_multiple_elements__then_last_element_removed =
  "pop_back__when_multiple_elements__then_last_element_removed" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers in
    (* when *)
    let result = pop_back test_object in
    (* then *)
    assert_equal ~printer:string_of_int (List.length numbers - 1) @@ length result ;
    Assert.assert_not_equal ~printer:string_of_int (back result) (list_last numbers)

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
