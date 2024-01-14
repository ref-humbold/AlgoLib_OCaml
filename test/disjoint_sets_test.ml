(* Tests: Disjoint sets structure (union-find) *)
open OUnit2
open Algolib.Disjoint_sets
open TestUtils

let numbers = [1; 2; 3; 4; 5; 6; 7; 8; 9]

module IntSets = Make (struct
    type t = int

    let compare = Stdlib.compare
  end)

(* size_Test_list *)

let size__when_empty__then_zero =
  "size When empty Then zero" >:: fun _ ->
    (* when *)
    let result = IntSets.size @@ IntSets.create () in
    (* then *)
    assert_equal ~printer:string_of_int 0 result

let size__when_not_empty__then_sets_count =
  "size When not empty Then sets count" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list numbers in
    (* when *)
    let result = IntSets.size test_object in
    (* then *)
    assert_equal ~printer:string_of_int (List.length numbers) result

let size_Test_list = test_list [size__when_empty__then_zero; size__when_not_empty__then_sets_count]

(* contains_Test_list *)

let contains__when_present__then_true =
  "contains When present Then true" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list numbers in
    (* when *)
    let result = IntSets.contains 4 test_object in
    (* then *)
    Assert.Bool.assert_true result

let contains__when_absent__then_false =
  "contains When absent Then false" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list numbers in
    (* when *)
    let result = IntSets.contains 18 test_object in
    (* then *)
    Assert.Bool.assert_false result

let contains_Test_list =
  test_list [contains__when_present__then_true; contains__when_absent__then_false]

(* add_Test_list *)

let add__when_new_elements__then_singleton_sets =
  "add When new elements Then singleton sets" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list numbers and element = 18 in
    (* when *)
    IntSets.add element test_object ;
    (* then *)
    Assert.Bool.assert_true @@ IntSets.contains element test_object ;
    assert_equal ~printer:string_of_int (List.length numbers + 1) @@ IntSets.size test_object

let add__when_present_element__then_element_present =
  "add When present element Then Element_present" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list numbers and element = 7 in
    (* when *)
    let exec () = IntSets.add element test_object in
    (* then *)
    assert_raises (IntSets.Element_present 7) exec

let add_Test_list =
  test_list
    [add__when_new_elements__then_singleton_sets; add__when_present_element__then_element_present]

(* add_list_Test_list *)

let add_list__when_new_elements__then_singleton_sets =
  "add_list When new elements Then singleton sets" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list numbers and elements = [20; 17; 35] in
    (* when *)
    IntSets.add_list elements test_object ;
    (* then *)
    List.iter (fun e -> Assert.Bool.assert_true @@ IntSets.contains e test_object) elements ;
    assert_equal ~printer:string_of_int (List.length numbers + List.length elements)
    @@ IntSets.size test_object

let add_list__when_present_element__then_element_present =
  "add_list When present element Then Element_present" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list numbers and elements = [20; 7; 35] in
    (* when *)
    let exec () = IntSets.add_list elements test_object in
    (* then *)
    assert_raises (IntSets.Element_present 7) exec

let add_list_Test_list =
  test_list
    [ add_list__when_new_elements__then_singleton_sets;
      add_list__when_present_element__then_element_present ]

(* add_seq_Test_list *)

let add_seq__when_new_elements__then_singleton_sets =
  "add_seq When new elements Then singleton sets" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list numbers
    and elements = Seq.cons 20 @@ Seq.cons 17 @@ Seq.return 35 in
    (* when *)
    IntSets.add_seq elements test_object ;
    (* then *)
    Seq.iter (fun e -> Assert.Bool.assert_true @@ IntSets.contains e test_object) elements ;
    assert_equal ~printer:string_of_int (List.length numbers + Seq.length elements)
    @@ IntSets.size test_object

let add_seq__when_present_element__then_element_present =
  "add_seq When present element Then Element_present" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list numbers
    and elements = Seq.cons 20 @@ Seq.cons 7 @@ Seq.return 35 in
    (* when *)
    let exec () = IntSets.add_seq elements test_object in
    (* then *)
    assert_raises (IntSets.Element_present 7) exec

let add_seq_Test_list =
  test_list
    [ add_seq__when_new_elements__then_singleton_sets;
      add_seq__when_present_element__then_element_present ]

(* find_set_Test_list *)

let find_set__when_present__then_represent =
  "find_set When present Then represent" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list numbers and element = 4 in
    (* when *)
    let result = IntSets.find_set element test_object in
    (* then *)
    assert_equal ~printer:string_of_int element result

let find_set__when_absent__then_not_found =
  "find_set When absent Then Not_found" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list numbers in
    (* when *)
    let exec () = IntSets.find_set 14 test_object in
    (* then *)
    assert_raises Not_found exec

let find_set_Test_list =
  test_list [find_set__when_present__then_represent; find_set__when_absent__then_not_found]

(* find_set_opt_Test_list *)

let find_set_opt__when_present__then_some_with_represent =
  "find_set_opt When present Then Some with represent" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list numbers and element = 4 in
    (* when *)
    let result = IntSets.find_set_opt element test_object in
    (* then *)
    Assert.Option.assert_some ~printer:string_of_int element result

let find_set_opt__when_absent__then_none =
  "find_set When absent Then None" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list numbers in
    (* when *)
    let result = IntSets.find_set_opt 14 test_object in
    (* then *)
    Assert.Option.assert_none ~printer:string_of_int result

let find_set_opt_Test_list =
  test_list
    [find_set_opt__when_present__then_some_with_represent; find_set_opt__when_absent__then_none]

(* union_set_Test_list *)

let union_set__when_different_sets__then_same_represent =
  "union_set When different sets Then same represent" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list numbers and element1 = 4 and element2 = 6 in
    (* when *)
    IntSets.union_set element1 element2 test_object ;
    (* then *)
    Assert.Bool.assert_true @@ IntSets.is_same_set element1 element2 test_object ;
    assert_equal
      ~printer:string_of_int
      (IntSets.find_set element1 test_object)
      (IntSets.find_set element2 test_object)

let union_set__when_single_element__then_same_represent =
  "union_set When single element Then same represent" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list numbers and element = 4 in
    (* when *)
    IntSets.union_set element element test_object ;
    (* then *)
    Assert.Bool.assert_true @@ IntSets.is_same_set element element test_object ;
    assert_equal
      ~printer:string_of_int
      (IntSets.find_set element test_object)
      (IntSets.find_set element test_object)

let union_set__when_new_elements_in_chain__then_same_represent =
  "union_set When new elements in chain Then same represent" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list numbers and elements = [20; 17; 35] in
    IntSets.add_list elements test_object ;
    (* when *)
    IntSets.union_set (List.hd elements) (List.nth elements 1) test_object ;
    IntSets.union_set (List.nth elements 1) (List.nth elements 2) test_object ;
    (* then *)
    Assert.Bool.assert_true
    @@ IntSets.is_same_set (List.hd elements) (List.nth elements 2) test_object ;
    assert_equal
      ~printer:string_of_int
      (IntSets.find_set (List.hd elements) test_object)
      (IntSets.find_set (List.nth elements 2) test_object)

let union_set__when_absent__then_not_found =
  "union_set When absent Then Not_found" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list numbers in
    (* when *)
    let exec () = IntSets.union_set 15 7 test_object in
    (* then *)
    assert_raises Not_found exec

let union_set_Test_list =
  test_list
    [ union_set__when_different_sets__then_same_represent;
      union_set__when_single_element__then_same_represent;
      union_set__when_new_elements_in_chain__then_same_represent;
      union_set__when_absent__then_not_found ]

(* is_same_set_Test_list *)

let is_same_set__when_different_sets__then_false =
  "is_same_set When different sets Then false" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list numbers in
    (* when *)
    let result = IntSets.is_same_set 4 6 test_object in
    (* then *)
    Assert.Bool.assert_false result

let is_same_set__when_same_element__then_true =
  "is_same_set When different sets Then true" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list numbers and element = 4 in
    (* when *)
    let result = IntSets.is_same_set element element test_object in
    (* then *)
    Assert.Bool.assert_true result

let is_same_set__when_same_set__then_true =
  "is_same_set When different sets Then true" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list numbers and element1 = 3 and element2 = 8 in
    IntSets.union_set element1 element2 test_object ;
    (* when *)
    let result = IntSets.is_same_set element2 element1 test_object in
    (* then *)
    Assert.Bool.assert_true result

let is_same_set__when_absent__then_not_found =
  "is_same_set When absent Then Not_found" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list numbers in
    (* when *)
    let exec () = IntSets.is_same_set 15 7 test_object in
    (* then *)
    assert_raises Not_found exec

let is_same_set_Test_list =
  test_list
    [ is_same_set__when_different_sets__then_false;
      is_same_set__when_same_element__then_true;
      is_same_set__when_same_set__then_true;
      is_same_set__when_absent__then_not_found ]

(* disjoint_sets_Test *)

let disjoint_sets_Test =
  "Tests: Disjoint sets structure (union-find)"
  >::: [ size_Test_list;
         contains_Test_list;
         find_set_Test_list;
         find_set_opt_Test_list;
         add_Test_list;
         add_list_Test_list;
         add_seq_Test_list;
         union_set_Test_list;
         is_same_set_Test_list ]

let _ = run_test_tt_main disjoint_sets_Test
