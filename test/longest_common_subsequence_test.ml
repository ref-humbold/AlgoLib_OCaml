(* Tests: Algorithm for longest common subsequence *)
open OUnit2
open Algolib.Longest_common_subsequence

let to_list s = List.of_seq @@ Seq.map Char.code (String.to_seq s)

(* count_lcs_length_Test_list *)

let count_lcs_length__when_empty__then_zero =
  "count_lcs_length When empty Then zero"
  >:: fun _ ->
    (* when *)
    let result = count_lcs_length_str "qwertyuiop" "" in
    (* then *)
    assert_equal ~printer:string_of_int 0 result

let count_lcs_length__when_repeated_single_element__then_one =
  "count_lcs_length When repeated single element Then one"
  >:: fun _ ->
    (* when *)
    let result = count_lcs_length_str "abcde" "eeee" in
    (* then *)
    assert_equal ~printer:string_of_int 1 result

let count_lcs_length__when_same_text__then_text_length =
  "count_lcs_length When same text Then text length"
  >:: fun _ ->
    (* given *)
    let text = "qwertyuiop" in
    (* when *)
    let result = count_lcs_length_str text text in
    (* then *)
    assert_equal ~printer:string_of_int (String.length text) result

let count_lcs_length__when_subtext__then_subtext_length =
  "count_lcs_length When subtext Then subtext length"
  >:: fun _ ->
    (* when *)
    let result = count_lcs_length_str "qwertyuiop" "zxqwertyasdfuiopcvb" in
    (* then *)
    assert_equal ~printer:string_of_int (String.length "qwertyuiop") result

let count_lcs_length__when_different__then_zero =
  "count_lcs_length When different Then zero"
  >:: fun _ ->
    (* when *)
    let result = count_lcs_length_str "qwertyuiop" "asdfghjkl" in
    (* then *)
    assert_equal ~printer:string_of_int 0 result

let count_lcs_length__when_common_subtext__then_common_subtext_length =
  "count_lcs_length When common subtext Then common subtext length"
  >:: fun _ ->
    (* when *)
    let result = count_lcs_length_str "qwertyuiop" "zxrtyasdfuiopcvb" in
    (* then *)
    assert_equal ~printer:string_of_int (String.length "rtyuiop") result

let count_lcs_length__when_same_sequence__then_sequence_length =
  "count_lcs_length When same sequence Then sequence length"
  >:: fun _ ->
    (* given*)
    let sequence = to_list "qwertyuiop" in
    (* when*)
    let result = count_lcs_length sequence sequence in
    (* then*)
    assert_equal ~printer:string_of_int (List.length sequence) result

let count_lcs_length__when_common_subsequence__then_common_subsequence_length =
  "count_lcs_length When common subsequence Then common subsequence length"
  >:: fun _ ->
    (*when*)
    let result = count_lcs_length (to_list "qwertyuiop") (to_list "zxrtyasdfuiopcvb") in
    (* then*)
    assert_equal ~printer:string_of_int (String.length "rtyuiop") result

let count_lcs_length_Test_list =
  test_list
    [ count_lcs_length__when_empty__then_zero;
      count_lcs_length__when_repeated_single_element__then_one;
      count_lcs_length__when_same_text__then_text_length;
      count_lcs_length__when_subtext__then_subtext_length;
      count_lcs_length__when_different__then_zero;
      count_lcs_length__when_common_subtext__then_common_subtext_length;
      count_lcs_length__when_same_sequence__then_sequence_length;
      count_lcs_length__when_common_subsequence__then_common_subsequence_length ]

(* longest_common_subsequence_Test *)

let longest_common_subsequence_Test =
  "Tests: Algorithm for longest common subsequence" >::: [count_lcs_length_Test_list]

let _ = run_test_tt_main longest_common_subsequence_Test
