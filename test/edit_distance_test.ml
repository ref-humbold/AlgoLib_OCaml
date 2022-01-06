(* Tests: Algorithms for edit distance *)

open OUnit2
open Algolib.Edit_distance

(* count_levenshtein_Test_list *)

let count_levenshtein__when_same_text__then_zero =
  "count_levenshtein When same text Then zero"
  >:: fun _ ->
    (* given *)
    let text = "qwertyuiop" in
    (* when *)
    let result = count_levenshtein text text in
    (* then *)
    assert_equal ~cmp:(cmp_float ~epsilon:1e-6) ~printer:string_of_float 0.0 result

let count_levenshtein__when_empty_source__then_sum_of_insertions =
  "count_levenshtein When empty source Then sum of insertions"
  >:: fun _ ->
    (* given *)
    let text = "qwertyuiop" and insertion_cost = 2.0 in
    (* when *)
    let result = count_levenshtein ~insertion_cost "" text in
    (* then *)
    assert_equal
      ~cmp:(cmp_float ~epsilon:1e-6)
      ~printer:string_of_float
      (float_of_int (String.length text) *. insertion_cost)
      result

let count_levenshtein__when_empty_destination__then_sum_of_deletions =
  "count_levenshtein When empty destination Then sum of deletions"
  >:: fun _ ->
    (* given *)
    let text = "qwertyuiop" and deletion_cost = 2.0 in
    (* when *)
    let result = count_levenshtein ~deletion_cost text "" in
    (* then *)
    assert_equal
      ~cmp:(cmp_float ~epsilon:1e-6)
      ~printer:string_of_float
      (float_of_int (String.length text) *. deletion_cost)
      result

let count_levenshtein__when_negative_cost__then_invalid_argument =
  "count_levenshtein When negative cost Then Invalid_argument"
  >:: fun _ ->
    (* when *)
    let exec () = count_levenshtein ~substitution_cost:(-1.0) "a" "b" in
    (* then *)
    assert_raises (Invalid_argument "Cost cannot be negative") exec

let count_levenshtein_Test_list =
  test_list
    [ count_levenshtein__when_same_text__then_zero;
      count_levenshtein__when_empty_source__then_sum_of_insertions;
      count_levenshtein__when_empty_destination__then_sum_of_deletions;
      count_levenshtein__when_negative_cost__then_invalid_argument ]

(* count_lcs_Test_list *)

let count_lcs__when_same_text__then_zero =
  "count_lcs When same text Then zero"
  >:: fun _ ->
    (* given *)
    let text = "qwertyuiop" in
    (* when *)
    let result = count_lcs text text in
    (* then *)
    assert_equal ~cmp:(cmp_float ~epsilon:1e-6) ~printer:string_of_float 0.0 result

let count_lcs__when_empty_source__then_sum_of_insertions =
  "count_lcs When empty source Then sum of insertions"
  >:: fun _ ->
    (* given *)
    let text = "qwertyuiop" and insertion_cost = 2.0 in
    (* when *)
    let result = count_lcs ~insertion_cost "" text in
    (* then *)
    assert_equal
      ~cmp:(cmp_float ~epsilon:1e-6)
      ~printer:string_of_float
      (float_of_int (String.length text) *. insertion_cost)
      result

let count_lcs__when_empty_destination__then_sum_of_deletions =
  "count_lcs When empty destination Then sum of deletions"
  >:: fun _ ->
    (* given *)
    let text = "qwertyuiop" and deletion_cost = 2.0 in
    (* when *)
    let result = count_lcs ~deletion_cost text "" in
    (* then *)
    assert_equal
      ~cmp:(cmp_float ~epsilon:1e-6)
      ~printer:string_of_float
      (float_of_int (String.length text) *. deletion_cost)
      result

let count_lcs__when_negative_cost__then_invalid_argument =
  "count_lcs When negative cost Then Invalid_argument"
  >:: fun _ ->
    (* when *)
    let exec () = count_lcs ~deletion_cost:(-1.0) "a" "b" in
    (* then *)
    assert_raises (Invalid_argument "Cost cannot be negative") exec

let count_lcs_Test_list =
  test_list
    [ count_lcs__when_same_text__then_zero; count_lcs__when_empty_source__then_sum_of_insertions;
      count_lcs__when_empty_destination__then_sum_of_deletions;
      count_lcs__when_negative_cost__then_invalid_argument ]

(* edit_distance_Test *)

let edit_distance_Test =
  "Tests: Algorithms for edit distance" >::: [count_levenshtein_Test_list; count_lcs_Test_list]

let _ = run_test_tt_main edit_distance_Test
