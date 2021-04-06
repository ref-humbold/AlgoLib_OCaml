(* Tests: Structure of trie *)
open OUnit2
open Algolib.Trie
open Utils

let texts = ["abcd"; "ab"; "xyz"]

(* size_Test_list *)

let size__when_empty__then_zero =
  "size When empty Then zero"
  >:: fun _ ->
    (* when *)
    let result = size empty in
    (* then *)
    assert_equal ~printer:string_of_int 0 result

let size__when_not_empty__then_sets_count =
  "size When not empty Then sets count"
  >:: fun _ ->
    (* given *)
    let test_object = of_list texts in
    (* when *)
    let result = size test_object in
    (* then *)
    assert_equal ~printer:string_of_int 3 result

let size_Test_list = test_list [size__when_empty__then_zero; size__when_not_empty__then_sets_count]

(* contains_Test_list *)

let contains__when_present__then_true =
  "contains When present Then true"
  >:: fun _ ->
    (* given *)
    let test_object = of_list texts in
    (* when *)
    let result = contains "abcd" test_object in
    (* then *)
    assert_bool Messages.true_value result

let contains__when_absent__then_false =
  "contains When absent Then false"
  >:: fun _ ->
    (* given *)
    let test_object = of_list texts in
    (* when *)
    let result = contains "abxx" test_object in
    (* then *)
    assert_bool Messages.false_value @@ not result

let contains__when_absent_prefix__then_false =
  "contains When absent prefix Then false"
  >:: fun _ ->
    (* given *)
    let test_object = of_list texts in
    (* when *)
    let result = contains "xy" test_object in
    (* then *)
    assert_bool Messages.false_value @@ not result

let contains_Test_list =
  test_list
    [ contains__when_present__then_true; contains__when_absent__then_false;
      contains__when_absent_prefix__then_false ]

(* trie_Test *)

let trie_Test = "Tests: Structure of trie" >::: [size_Test_list; contains_Test_list]

let _ = run_test_tt_main trie_Test
