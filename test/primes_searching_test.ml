(* Tests: Algorithms for searching for prime numbers. *)
open OUnit2
open Algolib.Primes_searching
open TestUtils

(* find_primes_Test_list *)

let find_primes__when_min_greater_than_max__then_empty =
  "find_primes__when_min_greater_than_max__then_empty" >:: fun _ ->
    (* when *)
    let result = find_primes 100 30 in
    (* then *)
    assert_equal ~printer:(Printers.list string_of_int) [] result

let find_primes__when_max_is_composite__then_all_primes =
  "find_primes__when_max_is_composite__then_all_primes" >:: fun _ ->
    (* when *)
    let result = find_primes 0 100 in
    (* then *)
    assert_equal
      ~printer:(Printers.list string_of_int)
      [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71; 73; 79; 83; 89; 97]
      result

let find_primes__when_max_is_prime__then_max_exclusive =
  "find_primes__when_max_is_prime__then_max_exclusive" >:: fun _ ->
    (* when *)
    let result = find_primes 0 67 in
    (* then *)
    assert_equal
      ~printer:(Printers.list string_of_int)
      [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61]
      result

let find_primes__when_max_is_two__then_empty =
  "find_primes__when_max_is_two__then_empty" >:: fun _ ->
    (* when *)
    let result = find_primes 0 2 in
    (* then *)
    assert_equal ~printer:(Printers.list string_of_int) [] result

let find_primes__when_max_is_three__then_single_element =
  "find_primes__when_max_is_three__then_single_element" >:: fun _ ->
    (* when *)
    let result = find_primes 0 3 in
    (* then *)
    assert_equal ~printer:(Printers.list string_of_int) [2] result

let find_primes__when_max_is_four__then_all_primes =
  "find_primes__when_max_is_four__then_all_primes" >:: fun _ ->
    (* when *)
    let result = find_primes 0 4 in
    (* then *)
    assert_equal ~printer:(Printers.list string_of_int) [2; 3] result

let find_primes__when_range__then_primes_between =
  "find_primes__when_range__then_primes_between" >:: fun _ ->
    (* when *)
    let result = find_primes 30 200 in
    (* then *)
    assert_equal
      ~printer:(Printers.list string_of_int)
      [ 31;
        37;
        41;
        43;
        47;
        53;
        59;
        61;
        67;
        71;
        73;
        79;
        83;
        89;
        97;
        101;
        103;
        107;
        109;
        113;
        127;
        131;
        137;
        139;
        149;
        151;
        157;
        163;
        167;
        173;
        179;
        181;
        191;
        193;
        197;
        199 ]
      result

let find_primes__when_minimum_is_two__then_two_included =
  "find_primes__when_minimum_is_two__then_two_included" >:: fun _ ->
    (* when *)
    let result = find_primes 2 30 in
    (* then *)
    assert_equal ~printer:(Printers.list string_of_int) [2; 3; 5; 7; 11; 13; 17; 19; 23; 29] result

let find_primes__when_minimum_is_three__then_two_not_included =
  "find_primes__when_minimum_is_three__then_two_not_included" >:: fun _ ->
    (* when *)
    let result = find_primes 3 30 in
    (* then *)
    assert_equal ~printer:(Printers.list string_of_int) [3; 5; 7; 11; 13; 17; 19; 23; 29] result

let find_primes__when_max_is_fourth_power_of_prime__then_all_primes_between =
  "find_primes__when_max_is_fourth_power_of_prime__then_all_primes_between" >:: fun _ ->
    (* when *)
    let result = find_primes 9 81 in
    (* then *)
    assert_equal
      ~printer:(Printers.list string_of_int)
      [11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71; 73; 79]
      result

let find_primes__when_min_is_less_than_square_root_of_max__then_primes_between =
  "find_primes__when_min_is_less_than_square_root_of_max__then_primes_between" >:: fun _ ->
    (* when *)
    let result = find_primes 5 150 in
    (* then *)
    assert_equal
      ~printer:(Printers.list string_of_int)
      [ 5;
        7;
        11;
        13;
        17;
        19;
        23;
        29;
        31;
        37;
        41;
        43;
        47;
        53;
        59;
        61;
        67;
        71;
        73;
        79;
        83;
        89;
        97;
        101;
        103;
        107;
        109;
        113;
        127;
        131;
        137;
        139;
        149 ]
      result

let find_primes__when_min_and_max_are_primes__then_min_inclusive_and_max_exclusive =
  "find_primes__when_min_and_max_are_primes__then_min_inclusive_and_max_exclusive" >:: fun _ ->
    (* when *)
    let result = find_primes 137 317 in
    (* then *)
    assert_equal
      ~printer:(Printers.list string_of_int)
      [ 137;
        139;
        149;
        151;
        157;
        163;
        167;
        173;
        179;
        181;
        191;
        193;
        197;
        199;
        211;
        223;
        227;
        229;
        233;
        239;
        241;
        251;
        257;
        263;
        269;
        271;
        277;
        281;
        283;
        293;
        307;
        311;
        313 ]
      result

let find_primes__when_min_equals_max_and_prime__then_empty =
  "find_primes__when_min_equals_max_and_prime__then_empty" >:: fun _ ->
    (* when *)
    let result = find_primes 41 41 in
    (* then *)
    assert_equal ~printer:(Printers.list string_of_int) [] result

let find_primes__when_min_equals_max_and_composite__then_empty =
  "find_primes__when_min_equals_max_and_composite__then_empty" >:: fun _ ->
    (* when *)
    let result = find_primes 91 91 in
    (* then *)
    assert_equal ~printer:(Printers.list string_of_int) [] result

let find_primes_Test_list =
  test_list
    [ find_primes__when_min_greater_than_max__then_empty;
      find_primes__when_max_is_composite__then_all_primes;
      find_primes__when_max_is_prime__then_max_exclusive;
      find_primes__when_max_is_two__then_empty;
      find_primes__when_max_is_three__then_single_element;
      find_primes__when_max_is_four__then_all_primes;
      find_primes__when_range__then_primes_between;
      find_primes__when_minimum_is_two__then_two_included;
      find_primes__when_minimum_is_three__then_two_not_included;
      find_primes__when_max_is_fourth_power_of_prime__then_all_primes_between;
      find_primes__when_min_is_less_than_square_root_of_max__then_primes_between;
      find_primes__when_min_and_max_are_primes__then_min_inclusive_and_max_exclusive;
      find_primes__when_min_equals_max_and_prime__then_empty;
      find_primes__when_min_equals_max_and_composite__then_empty ]

(* primes_testing_Test *)

let primes_searching_Test =
  "Tests: Algorithms for searching for prime numbers" >::: [find_primes_Test_list]

let _ = run_test_tt_main primes_searching_Test
