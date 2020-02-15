(* Tests: Disjoint sets structure (union-find) *)
open OUnit2
open Algolib.Disjoint_sets

module IntDisjointSets = Make (struct
    type t = int

    let compare = Stdlib.compare
  end)

module IDS = IntDisjointSets

(* size *)

let size_When_empty_structure_Then_zero =
  "size When empty structure Then zero"
  >:: fun _ ->
    let result = IDS.create () in
    assert_equal 0 @@ IDS.size result

let size_When_non_empty_structure_Then_sets_count =
  "size When non empty structure Then sets count"
  >:: fun _ ->
    let result = IDS.create () in
    IDS.add_list [1; 2; 3; 4] result ;
    assert_equal 4 @@ IDS.size result

let size_Test_list =
  test_list [size_When_empty_structure_Then_zero; size_When_non_empty_structure_Then_sets_count]

(* disjoint_sets test *)

let disjoint_sets_Test = "Tests for disjoint sets structure" >::: [size_Test_list]

let _ = run_test_tt_main disjoint_sets_Test
