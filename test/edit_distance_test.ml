(* Tests: Algorithms for edit distance. *)
open OUnit2
open Algolib.Text.Edit_distance
open TestUtils

let epsilon = 1e-6

(* count_levenshtein_Test_list *)

let count_levenshtein__when_different_text__then_distance =
  "count_levenshtein__when_different_text__then_distance" >:: fun _ ->
    (* given *)
    let source = "qwertyuiop" and destination = "wertzuiopsx" in
    (* when *)
    let result = count_levenshtein source destination in
    (* then *)
    Assert.Float.assert_close ~epsilon 4.0 result

let count_levenshtein__when_same_text__then_zero =
  "count_levenshtein__when_same_text__then_zero" >:: fun _ ->
    (* given *)
    let text = "qwertyuiop" in
    (* when *)
    let result = count_levenshtein text text in
    (* then *)
    Assert.Float.assert_close ~epsilon 0.0 result

let count_levenshtein__when_empty_source__then_sum_of_insertions =
  "count_levenshtein__when_empty_source__then_sum_of_insertions" >:: fun _ ->
    (* given *)
    let text = "qwertyuiop" and insertion_cost = 2.0 in
    (* when *)
    let result = count_levenshtein ~insertion_cost "" text in
    (* then *)
    Assert.Float.assert_close ~epsilon (float_of_int (String.length text) *. insertion_cost) result

let count_levenshtein__when_empty_destination__then_sum_of_deletions =
  "count_levenshtein__when_empty_destination__then_sum_of_deletions" >:: fun _ ->
    (* given *)
    let text = "qwertyuiop" and deletion_cost = 2.0 in
    (* when *)
    let result = count_levenshtein ~deletion_cost text "" in
    (* then *)
    Assert.Float.assert_close ~epsilon (float_of_int (String.length text) *. deletion_cost) result

let count_levenshtein__when_negative_cost__then_invalid_argument =
  "count_levenshtein__when_negative_cost__then_invalid_argument" >:: fun _ ->
    (* when *)
    let exec () = count_levenshtein ~substitution_cost:(-1.0) "a" "b" in
    (* then *)
    assert_raises (Invalid_argument "Cost cannot be negative") exec

let count_levenshtein_Test_list =
  test_list
    [ count_levenshtein__when_different_text__then_distance;
      count_levenshtein__when_same_text__then_zero;
      count_levenshtein__when_empty_source__then_sum_of_insertions;
      count_levenshtein__when_empty_destination__then_sum_of_deletions;
      count_levenshtein__when_negative_cost__then_invalid_argument ]

(* count_lcs_Test_list *)

let count_lcs__when_different_text__then_distance =
  "count_lcs__when_different_text__then_distance" >:: fun _ ->
    (* given *)
    let source = "qwertyuiop" and destination = "wertzuiopsx" in
    (* when *)
    let result = count_lcs source destination in
    (* then *)
    Assert.Float.assert_close ~epsilon 5.0 result

let count_lcs__when_same_text__then_zero =
  "count_lcs__when_same_text__then_zero" >:: fun _ ->
    (* given *)
    let text = "qwertyuiop" in
    (* when *)
    let result = count_lcs text text in
    (* then *)
    Assert.Float.assert_close ~epsilon 0.0 result

let count_lcs__when_empty_source__then_sum_of_insertions =
  "count_lcs__when_empty_source__then_sum_of_insertions" >:: fun _ ->
    (* given *)
    let text = "qwertyuiop" and insertion_cost = 2.0 in
    (* when *)
    let result = count_lcs ~insertion_cost "" text in
    (* then *)
    Assert.Float.assert_close ~epsilon (float_of_int (String.length text) *. insertion_cost) result

let count_lcs__when_empty_destination__then_sum_of_deletions =
  "count_lcs__when_empty_destination__then_sum_of_deletions" >:: fun _ ->
    (* given *)
    let text = "qwertyuiop" and deletion_cost = 2.0 in
    (* when *)
    let result = count_lcs ~deletion_cost text "" in
    (* then *)
    Assert.Float.assert_close ~epsilon (float_of_int (String.length text) *. deletion_cost) result

let count_lcs__when_negative_cost__then_invalid_argument =
  "count_lcs__when_negative_cost__then_invalid_argument" >:: fun _ ->
    (* when *)
    let exec () = count_lcs ~deletion_cost:(-1.0) "a" "b" in
    (* then *)
    assert_raises (Invalid_argument "Cost cannot be negative") exec

let count_lcs_Test_list =
  test_list
    [ count_lcs__when_different_text__then_distance;
      count_lcs__when_same_text__then_zero;
      count_lcs__when_empty_source__then_sum_of_insertions;
      count_lcs__when_empty_destination__then_sum_of_deletions;
      count_lcs__when_negative_cost__then_invalid_argument ]

(* count_hamming_Test_list *)

let count_hamming__when_different_text__then_distance =
  "count_hamming__when_different_text__then_distance" >:: fun _ ->
    (* given *)
    let source = "qwertyuiop" and destination = "qvertzuimp" and substitution_cost = 2.0 in
    (* when *)
    let result = count_hamming source destination ~substitution_cost in
    (* then *)
    Assert.Float.assert_close ~epsilon (3.0 *. substitution_cost) result

let count_hamming__when_empty__then_zero =
  "count_hamming__when_empty__then_zero" >:: fun _ ->
    (* when *)
    let result = count_hamming "" "" in
    (* then *)
    Assert.Float.assert_close ~epsilon 0.0 result

let count_hamming__when_same_text__then_zero =
  "count_hamming__when_same_text__then_zero" >:: fun _ ->
    (* given *)
    let text = "qwertyuiop" in
    (* when *)
    let result = count_hamming text text in
    (* then *)
    Assert.Float.assert_close ~epsilon 0.0 result

let count_hamming__when_different_length__then_invalid_argument =
  "count_hamming__when_different_length__then_invalid_argument" >:: fun _ ->
    (* when *)
    let exec () = count_hamming "qwerty" "asdf" in
    (* then *)
    assert_raises (Invalid_argument "Texts must have equal length") exec

let count_hamming__when_negative_cost__then_invalid_argument =
  "count_hamming__when_negative_cost__then_invalid_argument" >:: fun _ ->
    (* when *)
    let exec () = count_hamming ~substitution_cost:(-1.0) "a" "b" in
    (* then *)
    assert_raises (Invalid_argument "Cost cannot be negative") exec

let count_hamming_Test_list =
  test_list
    [ count_hamming__when_different_text__then_distance;
      count_hamming__when_empty__then_zero;
      count_hamming__when_same_text__then_zero;
      count_hamming__when_different_length__then_invalid_argument;
      count_hamming__when_negative_cost__then_invalid_argument ]

(* edit_distance_Test *)

let edit_distance_Test =
  "Tests: Algorithms for edit distance"
  >::: [count_levenshtein_Test_list; count_lcs_Test_list; count_hamming_Test_list]

let _ = run_test_tt_main edit_distance_Test
