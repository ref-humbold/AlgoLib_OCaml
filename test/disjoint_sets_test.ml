(* Tests: Disjoint sets structure (union-find) *)
open OUnit2
open Algolib.Disjoint_sets
open Utils

module IntDisjointSets = Make (struct
    type t = int

    let compare = Stdlib.compare
  end)

module IDS = IntDisjointSets

(* size_Test_list *)

let size_When_empty_Then_zero =
  "size When empty Then zero"
  >:: fun _ ->
    (* when *)
    let result = IDS.size @@ IDS.create () in
    (* then *)
    assert_equal ~printer:string_of_int 0 result

let size_When_not_empty_Then_sets_count =
  "size When not empty Then sets count"
  >:: fun _ ->
    (* given *)
    let test_object = IDS.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    (* when *)
    let result = IDS.size test_object in
    (* then *)
    assert_equal ~printer:string_of_int 9 result

let size_Test_list = test_list [size_When_empty_Then_zero; size_When_not_empty_Then_sets_count]

(* contains_Test_list *)

let contains_When_present_Then_true =
  "contains When present Then true"
  >:: fun _ ->
    (* given *)
    let test_object = IDS.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    (* when *)
    let result = IDS.contains 4 test_object in
    (* then *)
    assert_bool "Expected true value" result

let contains_When_absent_Then_false =
  "contains When absent Then false"
  >:: fun _ ->
    (* given *)
    let test_object = IDS.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    (* when *)
    let result = IDS.contains 18 test_object in
    (* then *)
    assert_bool "Expected false value" @@ not result

let contains_Test_list = test_list [contains_When_present_Then_true; contains_When_absent_Then_false]

(* add_list_Test_list *)

let add_list_Test_list = test_list []

(* add_seq_Test_list *)

let add_seq_Test_list = test_list []

(* find_set_Test_list *)

let find_set_When_present_Then_represent =
  "find_set When present Then represent"
  >:: fun _ ->
    (* given *)
    let test_object = IDS.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    let elem = 5 in
    (* when *)
    let result = IDS.find_set elem test_object in
    (* then *)
    assert_equal ~printer:string_of_int elem result

let find_set_When_absent_Then_not_found =
  "find_set When absent Then Not_found"
  >:: fun _ ->
    (* given *)
    let test_object = IDS.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    (* when *)
    let result () = IDS.find_set 12 test_object in
    (* then *)
    assert_raises Not_found result

let find_set_Test_list =
  test_list [find_set_When_present_Then_represent; find_set_When_absent_Then_not_found]

(* find_set_opt_Test_list *)

let find_set_opt_When_present_Then_some_with_represent =
  "find_set_opt When present Then Some with represent"
  >:: fun _ ->
    (* given *)
    let test_object = IDS.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    let elem = 5 in
    (* when *)
    let result = IDS.find_set_opt elem test_object in
    (* then *)
    assert_equal ~printer:(Printers.opt_str string_of_int) (Some elem) result

let find_set_opt_When_absent_Then_none =
  "find_set When absent Then None"
  >:: fun _ ->
    (* given *)
    let test_object = IDS.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    (* when *)
    let result = IDS.find_set_opt 12 test_object in
    (* then *)
    assert_equal ~printer:(Printers.opt_str string_of_int) None result

let find_set_opt_Test_list =
  test_list [find_set_opt_When_present_Then_some_with_represent; find_set_opt_When_absent_Then_none]

(* union_set_Test_list *)

let union_set_When_different_sets_Then_same_represent =
  "union_set When different sets Then same represent"
  >:: fun _ ->
    (* given *)
    let test_object = IDS.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    let elem1 = 4 and elem2 = 6 in
    (* when *)
    IDS.union_set elem1 elem2 test_object ;
    (* then *)
    assert_bool "Expected true value" @@ IDS.is_same_set elem1 elem2 test_object ;
    assert_equal
      ~printer:string_of_int
      (IDS.find_set elem1 test_object)
      (IDS.find_set elem2 test_object)

let union_set_When_absent_Then_not_found =
  "union_set When absent Then Not_found"
  >:: fun _ ->
    (* given *)
    let test_object = IDS.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    (* when *)
    let result () = IDS.union_set 15 7 test_object in
    (* then *)
    assert_raises Not_found result

let union_set_Test_list =
  test_list [union_set_When_different_sets_Then_same_represent; union_set_When_absent_Then_not_found]

(* is_same_set_Test_list *)

let is_same_set_When_different_sets_Then_false =
  "is_same_set When different sets Then false"
  >:: fun _ ->
    (* given *)
    let test_object = IDS.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    (* when *)
    let result = IDS.is_same_set 4 6 test_object in
    (* then *)
    assert_bool "Expected false value" @@ not result

let is_same_set_When_same_element_Then_true =
  "is_same_set When different sets Then true"
  >:: fun _ ->
    (* given *)
    let test_object = IDS.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    (* when *)
    let result = IDS.is_same_set 4 4 test_object in
    (* then *)
    assert_bool "Expected false value" result

let is_same_set_When_same_set_Then_true =
  "is_same_set When different sets Then true"
  >:: fun _ ->
    (* given *)
    let test_object = IDS.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    let elem1 = 3 and elem2 = 8 in
    IDS.union_set 3 8 test_object ;
    (* when *)
    let result = IDS.is_same_set elem1 elem2 test_object in
    (* then *)
    assert_bool "Expected false value" result

let is_same_set_When_absent_Then_not_found =
  "is_same_set When absent Then Not_found"
  >:: fun _ ->
    (* given *)
    let test_object = IDS.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9] in
    (* when *)
    let result () = IDS.is_same_set 15 7 test_object in
    (* then *)
    assert_raises Not_found result

let is_same_set_Test_list =
  test_list
    [ is_same_set_When_different_sets_Then_false; is_same_set_When_same_element_Then_true;
      is_same_set_When_same_set_Then_true; is_same_set_When_absent_Then_not_found ]

(* disjoint_sets_Test *)

let disjoint_sets_Test =
  "Tests: Disjoint sets structure (union-find)"
  >::: [ size_Test_list; contains_Test_list; find_set_Test_list; find_set_opt_Test_list;
         add_list_Test_list; add_seq_Test_list; union_set_Test_list; is_same_set_Test_list ]

let _ = run_test_tt_main disjoint_sets_Test
