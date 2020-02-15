(* Tests: Disjoint sets structure (union-find) *)
open OUnit2
open Algolib.Disjoint_sets

module IntDisjointSets = Make (struct
    type t = int

    let compare = Stdlib.compare
  end)

module IDS = IntDisjointSets

let test_object = IDS.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9]

(* size_Test_list *)

let size_When_empty_Then_zero =
  "size When empty Then zero"
  >:: fun _ ->
    let result = IDS.size @@ IDS.create () in
    assert_equal ~printer:string_of_int 0 result

let size_When_not_empty_Then_sets_count =
  "size When not empty Then sets count"
  >:: fun _ ->
    let result = IDS.size test_object in
    assert_equal ~printer:string_of_int 9 result

let size_Test_list = test_list [size_When_empty_Then_zero; size_When_not_empty_Then_sets_count]

(* contains_Test_list *)

let contains_When_present_Then_true =
  "contains When present Then true"
  >:: fun _ ->
    let result = IDS.contains 4 test_object in
    assert_bool "Expected true value" result

let contains_When_absent_Then_false =
  "contains When absent Then false"
  >:: fun _ ->
    let result = IDS.contains 18 test_object in
    assert_bool "Expected false value" @@ not result

let contains_Test_list = test_list [contains_When_present_Then_true; contains_When_absent_Then_false]

(* find_set_Test_list *)

let find_set_When_present_Then_represent =
  "find_set When present Then represent"
  >:: fun _ ->
    let elem = 5 in
    let result = IDS.find_set elem test_object in
    assert_equal ~printer:string_of_int elem result

let find_set_When_absent_Then_not_found =
  "find_set When absent Then Not_found"
  >:: fun _ ->
    let result () = IDS.find_set 12 test_object in
    assert_raises Not_found result

let find_set_Test_list =
  test_list [find_set_When_present_Then_represent; find_set_When_absent_Then_not_found]

(* disjoint_sets_Test *)

let disjoint_sets_Test =
  "Tests: Disjoint sets structure (union-find)"
  >::: [size_Test_list; contains_Test_list; find_set_Test_list]

let _ = run_test_tt_main disjoint_sets_Test
