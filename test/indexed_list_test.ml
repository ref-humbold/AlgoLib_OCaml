(* Tests: Structure of indexed list *)
open OUnit2
open Algolib.Indexed_list
open TestUtils

let numbers = [10; 6; 14; 97; 24; 37; 2; 30; 45; 18; 51; 71; 68; 26]

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

(* head_Test_list *)

let head__when_empty__then_empty_list =
  "head__when_empty__then_empty_list" >:: fun _ ->
    (* given *)
    let test_object = empty in
    (* when *)
    let exec () = head test_object in
    (* then *)
    assert_raises Empty_list exec

let head__when_not_empty__then_first_element =
  "head__when_not_empty__then_first_element" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers in
    (* when *)
    let result = head test_object in
    (* then *)
    assert_equal ~printer:string_of_int (List.hd numbers) result

let head_Test_list =
  test_list [head__when_empty__then_empty_list; head__when_not_empty__then_first_element]

(* tail_Test_list *)

let tail__when_empty__then_empty_list =
  "tail__when_empty__then_empty_list" >:: fun _ ->
    (* given *)
    let test_object = empty in
    (* when *)
    let exec () = tail test_object in
    (* then *)
    assert_raises Empty_list exec

let tail__when_single_element__then_head_removed =
  "tail__when_single_element__then_head_removed" >:: fun _ ->
    (* given *)
    let test_object = of_seq @@ Seq.return 19 in
    (* when *)
    let result = tail test_object in
    (* then *)
    Assert.Bool.assert_true @@ is_empty result

let tail__when_multiple_elements__then_head_removed =
  "tail__when_multiple_elements__then_head_removed" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers in
    (* when *)
    let result = tail test_object in
    (* then *)
    assert_equal ~printer:string_of_int (List.length numbers - 1) @@ length result ;
    Assert.assert_not_equal ~printer:string_of_int (List.hd numbers) @@ head result

let tail_Test_list =
  test_list
    [ tail__when_empty__then_empty_list;
      tail__when_single_element__then_head_removed;
      tail__when_multiple_elements__then_head_removed ]

(* cons_Test_list *)

let cons__when_new_element__then_added =
  "cons__when_new_element__then_added" >:: fun _ ->
    (* given *)
    let element = 111 and test_object = of_list numbers in
    (* when *)
    let result = element @:: test_object in
    (* then *)
    assert_equal ~printer:string_of_int (List.length numbers + 1) @@ length result ;
    assert_equal ~printer:string_of_int element @@ head result ;
    assert_equal ~printer:string_of_int (List.hd numbers) @@ head (tail result)

let cons__when_multiple_elements__then_added =
  "cons__when_multiple_elements__then_added" >:: fun _ ->
    (* given *)
    let elements = [111; 140; 187] and test_object = of_list numbers in
    (* when *)
    let result =
      List.nth elements 0 @:: List.nth elements 1 @:: List.nth elements 2 @:: test_object
    in
    (* then *)
    assert_equal ~printer:string_of_int (List.length numbers + List.length elements) @@ length result ;
    List.iteri (fun i e -> assert_equal ~printer:string_of_int e @@ get i result) elements ;
    assert_equal ~printer:string_of_int (List.hd numbers) @@ get (List.length elements) result

let cons_Test_list =
  test_list [cons__when_new_element__then_added; cons__when_multiple_elements__then_added]

(* get_Test_list *)

let get__when_empty__then_invalid_index =
  "get__when_empty__then_invalid_index" >:: fun _ ->
    (* given *)
    let index = 0 and test_object = empty in
    (* when *)
    let exec () = test_object &! index in
    (* then *)
    assert_raises (Invalid_index index) exec

let get__when_index_zero__then_head =
  "get__when_index_zero__then_head" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers in
    (* when *)
    let result = test_object &! 0 in
    (* then *)
    assert_equal ~printer:string_of_int (List.hd numbers) result

let get__when_index_inside__then_element =
  "get__when_index_inside__then_element" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers in
    List.iter
      (fun index ->
         (* when *)
         let result = test_object &! index in
         (* then *)
         assert_equal ~printer:string_of_int (List.nth numbers index) result )
      [4; 13; 1; 12; 9]

let get__when_index_exceeds_length__then_invalid_index =
  "get__when_index_exceeds_length__then_invalid_index" >:: fun _ ->
    (* given *)
    let index = List.length numbers + 3 and test_object = of_list numbers in
    (* when *)
    let exec () = test_object &! index in
    (* then *)
    assert_raises (Invalid_index index) exec

let get__when_index_negative__then_invalid_index =
  "get__when_index_negative__then_invalid_index" >:: fun _ ->
    (* given *)
    let index = -3 and test_object = of_list numbers in
    (* when *)
    let exec () = test_object &! index in
    (* then *)
    assert_raises (Invalid_index index) exec

let get_Test_list =
  test_list
    [ get__when_empty__then_invalid_index;
      get__when_index_zero__then_head;
      get__when_index_inside__then_element;
      get__when_index_exceeds_length__then_invalid_index;
      get__when_index_negative__then_invalid_index ]

(* set_Test_list *)

let set__when_empty__then_invalid_index =
  "set__when_empty__then_invalid_index" >:: fun _ ->
    (* given *)
    let index = 0 and test_object = empty in
    (* when *)
    let exec () = set index 111 test_object in
    (* then *)
    assert_raises (Invalid_index index) exec

let set__when_index_zero__then_head_changed =
  "set__when_index_zero__then_head_changed" >:: fun _ ->
    (* given *)
    let element = 111 and test_object = of_list numbers in
    (* when *)
    let result = set 0 element test_object in
    (* then *)
    assert_equal ~printer:string_of_int element @@ head result

let set__when_index_inside__then_element_changed =
  "set__when_index_inside__then_element_changed" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers in
    List.iter
      (fun (index, elem) ->
         (* when *)
         let result = set index elem test_object in
         (* then *)
         assert_equal ~printer:string_of_int elem @@ get index result )
      [(4, 111); (13, 140); (1, 187); (12, 234); (9, 275)]

let set__when_index_exceeds_length__then_invalid_index =
  "set__when_index_exceeds_length__then_invalid_index" >:: fun _ ->
    (* given *)
    let index = List.length numbers + 3 and test_object = of_list numbers in
    (* when *)
    let exec () = set index 111 test_object in
    (* then *)
    assert_raises (Invalid_index index) exec

let set__when_index_negative__then_invalid_index =
  "set__when_index_negative__then_invalid_index" >:: fun _ ->
    (* given *)
    let index = -3 and test_object = of_list numbers in
    (* when *)
    let exec () = set index 111 test_object in
    (* then *)
    assert_raises (Invalid_index index) exec

let set_Test_list =
  test_list
    [ set__when_empty__then_invalid_index;
      set__when_index_zero__then_head_changed;
      set__when_index_inside__then_element_changed;
      set__when_index_exceeds_length__then_invalid_index;
      set__when_index_negative__then_invalid_index ]

(* to_seq_Test_list *)

let to_seq__when_empty__then_empty_list =
  "to_seq__when_empty__then_empty_list" >:: fun _ ->
    (* given *)
    let test_object = empty in
    (* when *)
    let result = to_seq test_object in
    (* then *)
    assert_equal ~printer:(Printers.list string_of_int) [] @@ List.of_seq result

let to_seq__when_not_empty__then_all_elements =
  "to_seq__when_not_empty__then_all_elements" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers in
    (* when *)
    let result = to_seq test_object in
    (* then *)
    assert_equal ~printer:(Printers.list string_of_int) numbers @@ List.of_seq result

let to_seq_Test_list =
  test_list [to_seq__when_empty__then_empty_list; to_seq__when_not_empty__then_all_elements]

(* to_list_Test_list *)

let to_list__when_empty__then_empty_list =
  "to_list__when_empty__then_empty_list" >:: fun _ ->
    (* given *)
    let test_object = empty in
    (* when *)
    let result = to_list test_object in
    (* then *)
    assert_equal ~printer:(Printers.list string_of_int) [] result

let to_list__when_not_empty__then_all_elements =
  "to_list__when_not_empty__then_all_elements" >:: fun _ ->
    (* given *)
    let test_object = of_list numbers in
    (* when *)
    let result = to_list test_object in
    (* then *)
    assert_equal ~printer:(Printers.list string_of_int) numbers result

let to_list_Test_list =
  test_list [to_list__when_empty__then_empty_list; to_list__when_not_empty__then_all_elements]

(* indexed_list_heap_Test *)

let indexed_list_Test =
  "Tests: Structure of indexed_list"
  >::: [ is_empty_Test_list;
         length_Test_list;
         head_Test_list;
         tail_Test_list;
         cons_Test_list;
         get_Test_list;
         set_Test_list;
         to_seq_Test_list;
         to_list_Test_list ]

let _ = run_test_tt_main indexed_list_Test
