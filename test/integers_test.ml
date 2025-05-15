(* Tests: Algorithms for basic computations on integers. *)
open OUnit2
open OAssert
open Algolib.Maths.Integers

(* gcd_Test_list *)

let gcd__when_numbers_are_composite__then_returns_gcd =
  "gcd__when_numbers_are_composite__then_returns_gcd" >:: fun _ ->
    (* when *)
    let result = 161 **/ 46 in
    (* then *)
    assert_that result @@ Is.Int.equal_to 23

let gcd__when_numbers_are_prime__then_returns_one =
  "gcd__when_numbers_are_prime__then_returns_one" >:: fun _ ->
    (* when *)
    let result = 127 **/ 41 in
    (* then *)
    assert_that result @@ Is.Int.equal_to 1

let gcd__when_numbers_are_mutually_prime__then_returns_one =
  "gcd__when_numbers_are_mutually_prime__then_returns_one" >:: fun _ ->
    (* when *)
    let result = 119 **/ 57 in
    (* then *)
    assert_that result @@ Is.Int.equal_to 1

let gcd__when_number1_is_multiple_of_number2__then_returns_number2 =
  "gcd__when_number1_is_multiple_of_number2__then_returns_number2" >:: fun _ ->
    (* given *)
    let number1 = 272 and number2 = 34 in
    (* when *)
    let result = number1 **/ number2 in
    (* then *)
    assert_that result @@ Is.Int.equal_to number2

let gcd__when_number2_is_zero__then_returns_number1 =
  "gcd__when_number2_is_zero__then_returns_number1" >:: fun _ ->
    (* given *)
    let number1 = 96 and number2 = 0 in
    (* when *)
    let result = number1 **/ number2 in
    (* then *)
    assert_that result @@ Is.Int.equal_to number1

let gcd_Test_list =
  test_list
    [ gcd__when_numbers_are_composite__then_returns_gcd;
      gcd__when_numbers_are_prime__then_returns_one;
      gcd__when_numbers_are_mutually_prime__then_returns_one;
      gcd__when_number1_is_multiple_of_number2__then_returns_number2;
      gcd__when_number2_is_zero__then_returns_number1 ]

(* lcm_Test_list *)

let lcm__when_numbers_are_composite__then_returns_lcm =
  "lcm__when_numbers_are_composite__then_returns_lcm" >:: fun _ ->
    (* when *)
    let result = 161 **^ 46 in
    (* then *)
    assert_that result @@ Is.Int.equal_to 322

let lcm__when_numbers_are_prime__then_returns_product =
  "lcm__when_numbers_are_prime__then_returns_product" >:: fun _ ->
    (* when *)
    let result = 127 **^ 41 in
    (* then *)
    assert_that result @@ Is.Int.equal_to 5207

let lcm__when_numbers_are_mutually_prime__then_returns_product =
  "lcm__when_numbers_are_mutually_prime__then_returns_product" >:: fun _ ->
    (* when *)
    let result = 119 **^ 57 in
    (* then *)
    assert_that result @@ Is.Int.equal_to 6783

let lcm__when_number1_is_multiple_of_number2__then_returns_number1 =
  "lcm__when_number1_is_multiple_of_number2__then_returns_number1" >:: fun _ ->
    (* given *)
    let number1 = 272 and number2 = 34 in
    (* when *)
    let result = number1 **^ number2 in
    (* then *)
    assert_that result @@ Is.Int.equal_to number1

let lcm__when_number2_is_zero__then_returns_zero =
  "lcm__when_number2_is_zero__then_returns_zero" >:: fun _ ->
    (* when *)
    let result = 96 **^ 0 in
    (* then *)
    assert_that result @@ Is.Int.zero

let lcm_Test_list =
  test_list
    [ lcm__when_numbers_are_composite__then_returns_lcm;
      lcm__when_numbers_are_prime__then_returns_product;
      lcm__when_numbers_are_mutually_prime__then_returns_product;
      lcm__when_number1_is_multiple_of_number2__then_returns_number1;
      lcm__when_number2_is_zero__then_returns_zero ]

(* multiply_Test_list *)

let multiply__when_factor1_is_zero__then_returns_zero =
  "multiply__when_factor1_is_zero__then_returns_zero" >:: fun _ ->
    (* when *)
    let result = 0 *! 14 in
    (* then *)
    assert_that result @@ Is.Int.zero

let multiply__when_factor2_is_zero__then_returns_zero =
  "multiply__when_factor2_is_zero__then_returns_zero" >:: fun _ ->
    (* when *)
    let result = 14 *! 0 in
    (* then *)
    assert_that result @@ Is.Int.zero

let multiply__when_factors_are_zero__then_returns_zero =
  "multiply__when_factors_are_zero__then_returns_zero" >:: fun _ ->
    (* when *)
    let result = 0 *! 0 in
    (* then *)
    assert_that result @@ Is.Int.zero

let multiply__when_factor1_is_negative_and_factor2_is_positive =
  "multiply__when_factor1_is_negative_and_factor2_is_positive" >:: fun _ ->
    (* when *)
    let result = -3 *! 10 in
    (* then *)
    assert_that result @@ Is.Int.equal_to (-30)

let multiply__when_factor1_is_positive_and_factor2_is_negative =
  "multiply__when_factor1_is_positive_and_factor2_is_negative" >:: fun _ ->
    (* when *)
    let result = 3 *! -10 in
    (* then *)
    assert_that result @@ Is.Int.equal_to (-30)

let multiply__when_factors_are_negative =
  "multiply__when_factors_are_negative" >:: fun _ ->
    let result = -3 *! -10 in
    (* then *)
    assert_that result @@ Is.Int.equal_to 30

let multiply_mod__when_modulo_and_factors_are_positive =
  "multiply_mod__when_modulo_and_factors_are_positive" >:: fun _ ->
    (* when *)
    let result = multiply_mod 547 312 10000 in
    (* then *)
    assert_that result @@ Is.Int.equal_to 664

let multiply_mod__when_modulo_is_positive_and_factor1_is_negative =
  "multiply_mod__when_modulo_is_positive_and_factor1_is_negative" >:: fun _ ->
    (* when *)
    let result = multiply_mod (-547) 312 10000 in
    (* then *)
    assert_that result @@ Is.Int.equal_to 9336

let multiply_mod__when_modulo_is_positive_and_factor2_is_negative =
  "multiply_mod__when_modulo_is_positive_and_factor2_is_negative" >:: fun _ ->
    (* when *)
    let result = multiply_mod 547 (-312) 10000 in
    (* then *)
    assert_that result @@ Is.Int.equal_to 9336

let multiply_mod__when_modulo_is_positive_and_factors_are_negative =
  "multiply_mod__when_modulo_is_positive_and_factors_are_negative" >:: fun _ ->
    (* when *)
    let result = multiply_mod (-547) (-312) 10000 in
    (* then *)
    assert_that result @@ Is.Int.equal_to 664

let multiply_mod__when_modulo_is_negative__then_failure =
  "multiply_mod__when_modulo_is_negative__then_failure" >:: fun _ ->
    (* when *)
    let exec () = multiply_mod 547 312 (-10000) in
    (* then *)
    assert_raises (Failure "Non-positive modulo") exec

let multiply_Test_list =
  test_list
    [ multiply__when_factor1_is_zero__then_returns_zero;
      multiply__when_factor2_is_zero__then_returns_zero;
      multiply__when_factors_are_zero__then_returns_zero;
      multiply__when_factor1_is_negative_and_factor2_is_positive;
      multiply__when_factor1_is_positive_and_factor2_is_negative;
      multiply__when_factors_are_negative;
      multiply_mod__when_modulo_and_factors_are_positive;
      multiply_mod__when_modulo_is_positive_and_factor1_is_negative;
      multiply_mod__when_modulo_is_positive_and_factor2_is_negative;
      multiply_mod__when_modulo_is_positive_and_factors_are_negative;
      multiply_mod__when_modulo_is_negative__then_failure ]

(* power_Test_list *)

let power__when_base_is_zero__then_returns_zero =
  "power__when_base_is_zero__then_returns_zero" >:: fun _ ->
    (* when *)
    let result = 0 **! 14 in
    (* then *)
    assert_that result @@ Is.Int.zero

let power__when_exponent_is_zero__then_returns_one =
  "power__when_exponent_is_zero__then_returns_one" >:: fun _ ->
    (* when *)
    let result = 14 **! 0 in
    (* then *)
    assert_that result @@ Is.Int.equal_to 1

let power__when_base_and_exponent_are_zero__then_failure =
  "power__when_base_and_exponent_are_zero__then_failure" >:: fun _ ->
    (* when *)
    let exec () = 0 **! 0 in
    (* then *)
    assert_raises (Failure "Not a number") exec

let power__when_base_and_exponent_are_positive =
  "power__when_base_and_exponent_are_positive" >:: fun _ ->
    (* when *)
    let result = 3 **! 10 in
    (* then *)
    assert_that result @@ Is.Int.equal_to 59049

let power__when_base_is_negative_and_exponent_is_even =
  "power__when_base_is_negative_and_exponent_is_even" >:: fun _ ->
    (* when *)
    let result = -3 **! 10 in
    (* then *)
    assert_that result @@ Is.Int.equal_to 59049

let power__when_base_is_negative_and_exponent_is_odd =
  "power__when_base_is_negative_and_exponent_is_odd" >:: fun _ ->
    (* when *)
    let result = -3 **! 9 in
    (* then *)
    assert_that result @@ Is.Int.equal_to (-19683)

let power__when_exponent_is_negative__then_failure =
  "power__when_exponent_is_negative__then_failure" >:: fun _ ->
    (* when *)
    let exec () = 3 **! -10 in
    (* then *)
    assert_raises (Failure "Negative exponent") exec

let power_mod__when_modulo_and_base_are_positive =
  "power_mod__when_modulo_and_base_are_positive" >:: fun _ ->
    (* when *)
    let result = power_mod 5 11 10000 in
    (* then *)
    assert_that result @@ Is.Int.equal_to 8125

let power_mod__when_modulo_is_positive_and_base_is_negative =
  "power_mod__when_modulo_is_positive_and_base_is_negative" >:: fun _ ->
    (* when *)
    let result = power_mod (-5) 11 10000 in
    (* then *)
    assert_that result @@ Is.Int.equal_to 1875

let power_mod__when_modulo_is_negative__then_failure =
  "power_mod__when_modulo_is_negative__then_failure" >:: fun _ ->
    (* when *)
    let exec () = power_mod 5 11 (-10000) in
    (* then *)
    assert_raises (Failure "Non-positive modulo") exec

let power_Test_list =
  test_list
    [ power__when_base_is_zero__then_returns_zero;
      power__when_exponent_is_zero__then_returns_one;
      power__when_base_and_exponent_are_zero__then_failure;
      power__when_base_and_exponent_are_positive;
      power__when_base_is_negative_and_exponent_is_even;
      power__when_base_is_negative_and_exponent_is_odd;
      power__when_exponent_is_negative__then_failure;
      power_mod__when_modulo_and_base_are_positive;
      power_mod__when_modulo_is_positive_and_base_is_negative;
      power_mod__when_modulo_is_negative__then_failure ]

(* maths_Test *)

let maths_Test =
  "Tests: Algorithms for basic computations on integers"
  >::: [gcd_Test_list; lcm_Test_list; multiply_Test_list; power_Test_list]

let _ = run_test_tt_main maths_Test
