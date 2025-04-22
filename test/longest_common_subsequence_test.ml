(* Tests: Algorithm for longest common subsequence. *)
open OUnit2
open Algolib.Sequences.Longest_common_subsequence

let chars_of_string s = List.of_seq @@ Seq.map Char.code @@ String.to_seq s

(* count_lcs_length_Test_list *)

let count_lcs_length__when_empty__then_zero =
  "count_lcs_length__when_empty__then_zero" >:: fun _ ->
    (* when *)
    let result = count_lcs_length_str "qwertyuiop" "" in
    (* then *)
    assert_equal ~printer:string_of_int 0 result

let count_lcs_length__when_repeated_single_element__then_one =
  "count_lcs_length__when_repeated_single_element__then_one" >:: fun _ ->
    (* when *)
    let result = count_lcs_length_str "abcde" "eeee" in
    (* then *)
    assert_equal ~printer:string_of_int 1 result

let count_lcs_length__when_same_text__then_text_length =
  "count_lcs_length__when_same_text__then_text_length" >:: fun _ ->
    (* given *)
    let text = "qwertyuiop" in
    (* when *)
    let result = count_lcs_length_str text text in
    (* then *)
    assert_equal ~printer:string_of_int (String.length text) result

let count_lcs_length__when_subtext__then_subtext_length =
  "count_lcs_length__when_subtext__then_subtext_length" >:: fun _ ->
    (* when *)
    let result = count_lcs_length_str "qwertyuiop" "zxqwertyasdfuiopcvb" in
    (* then *)
    assert_equal ~printer:string_of_int (String.length "qwertyuiop") result

let count_lcs_length__when_different__then_zero =
  "count_lcs_length__when_different__then_zero" >:: fun _ ->
    (* when *)
    let result = count_lcs_length_str "qwertyuiop" "asdfghjkl" in
    (* then *)
    assert_equal ~printer:string_of_int 0 result

let count_lcs_length__when_common_subtext__then_common_subtext_length =
  "count_lcs_length__when_common_subtext__then_common_subtext_length" >:: fun _ ->
    (* when *)
    let result = count_lcs_length_str "qwertyuiop" "zxrtyasdfuiopcvb" in
    (* then *)
    assert_equal ~printer:string_of_int (String.length "rtyuiop") result

let count_lcs_length__when_same_sequence__then_sequence_length =
  "count_lcs_length__when_same_sequence__then_sequence_length" >:: fun _ ->
    (* given*)
    let sequence = chars_of_string "qwertyuiop" in
    (* when*)
    let result = count_lcs_length sequence sequence in
    (* then*)
    assert_equal ~printer:string_of_int (List.length sequence) result

let count_lcs_length__when_common_subsequence__then_common_subsequence_length =
  "count_lcs_length__when_common_subsequence__then_common_subsequence_length" >:: fun _ ->
    (*when*)
    let result =
      count_lcs_length (chars_of_string "qwertyuiop") (chars_of_string "zxrtyasdfuiopcvb")
    in
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
