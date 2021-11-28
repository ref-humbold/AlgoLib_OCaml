(* Tests: Basic mathematics algorithms *)
open OUnit2
open Algolib.Maths

(* gcd_Test_list *)

let gcd__when_numbers_are_composite__then_returns_gcd =
  "gcd When numbers are composite Then returns gcd"
  >:: fun _ ->
    (* when *)
    let result = 161 **/ 46 in
    (* then *)
    assert_equal ~printer:string_of_int 23 result

let gcd__when_numbers_are_prime__then_returns_one =
  "gcd When numbers are prime Then returns one"
  >:: fun _ ->
    (* when *)
    let result = 127 **/ 41 in
    (* then *)
    assert_equal ~printer:string_of_int 1 result

let gcd__when_numbers_are_mutually_prime__then_returns_one =
  "gcd When numbers are mutually prime Then returns one"
  >:: fun _ ->
    (* when *)
    let result = 119 **/ 57 in
    (* then *)
    assert_equal ~printer:string_of_int 1 result

let gcd__when_number1_is_multiple_of_number2__then_returns_number2 =
  "gcd When number1 is multiple of number2 Then returns number2"
  >:: fun _ ->
    (* given *)
    let number1 = 272 and number2 = 34 in
    (* when *)
    let result = number1 **/ number2 in
    (* then *)
    assert_equal ~printer:string_of_int number2 result

let gcd__when_number2_is_zero__then_returns_number1 =
  "gcd When number2 is zero Then returns number1"
  >:: fun _ ->
    (* given *)
    let number1 = 96 and number2 = 0 in
    (* when *)
    let result = number1 **/ number2 in
    (* then *)
    assert_equal ~printer:string_of_int number1 result

let gcd_Test_list =
  test_list
    [ gcd__when_numbers_are_composite__then_returns_gcd;
      gcd__when_numbers_are_prime__then_returns_one;
      gcd__when_numbers_are_mutually_prime__then_returns_one;
      gcd__when_number1_is_multiple_of_number2__then_returns_number2;
      gcd__when_number2_is_zero__then_returns_number1 ]

(* lcm_Test_list *)

let lcm__when_numbers_are_composite__then_returns_lcm =
  "lcm When numbers are composite Then returns lcm"
  >:: fun _ ->
    (* when *)
    let result = 161 **^ 46 in
    (* then *)
    assert_equal ~printer:string_of_int 322 result

let lcm__when_numbers_are_prime__then_returns_product =
  "lcm When numbers are prime Then returns product"
  >:: fun _ ->
    (* when *)
    let result = 127 **^ 41 in
    (* then *)
    assert_equal ~printer:string_of_int 5207 result

let lcm__when_numbers_are_mutually_prime__then_returns_product =
  "lcm When numbers are mutually prime Then returns product"
  >:: fun _ ->
    (* when *)
    let result = 119 **^ 57 in
    (* then *)
    assert_equal ~printer:string_of_int 6783 result

let lcm__when_number1_is_multiple_of_number2__then_returns_number1 =
  "lcm When number1 is multiple of number2 Then returns number1"
  >:: fun _ ->
    (* given *)
    let number1 = 272 and number2 = 34 in
    (* when *)
    let result = number1 **^ number2 in
    (* then *)
    assert_equal ~printer:string_of_int number1 result

let lcm__when_number2_is_zero__then_returns_zero =
  "lcm When number2 is zero Then returns zero"
  >:: fun _ ->
    (* when *)
    let result = 96 **^ 0 in
    (* then *)
    assert_equal ~printer:string_of_int 0 result

let lcm_Test_list =
  test_list
    [ lcm__when_numbers_are_composite__then_returns_lcm;
      lcm__when_numbers_are_prime__then_returns_product;
      lcm__when_numbers_are_mutually_prime__then_returns_product;
      lcm__when_number1_is_multiple_of_number2__then_returns_number1;
      lcm__when_number2_is_zero__then_returns_zero ]

(* multiply_Test_list *)

let multiply__when_factor1_is_zero__then_returns_zero =
  "multiply When factor1 is zero Then returns zero"
  >:: fun _ ->
    (* when *)
    let result = multiply 0 14 in
    (* then *)
    assert_equal ~printer:string_of_int 0 result

let multiply__when_factor2_is_zero__then_returns_zero =
  "multiply When factor2 is zero Then returns zero"
  >:: fun _ ->
    (* when *)
    let result = multiply 14 0 in
    (* then *)
    assert_equal ~printer:string_of_int 0 result

let multiply__when_factors_are_zero__then_returns_zero =
  "multiply When factors are zero Then returns zero"
  >:: fun _ ->
    (* when *)
    let result = multiply 0 0 in
    (* then *)
    assert_equal ~printer:string_of_int 0 result

let multiply__when_factor1_is_negative_and_factor2_is_positive =
  "multiply When factor1 is negative and factor2 is positive"
  >:: fun _ ->
    (* when *)
    let result = multiply (-3) 10 in
    (* then *)
    assert_equal ~printer:string_of_int (-30) result

let multiply__when_factor1_is_positive_and_factor2_is_negative =
  "multiply When factor1 is positive and factor2 is negative"
  >:: fun _ ->
    (* when *)
    let result = multiply 3 (-10) in
    (* then *)
    assert_equal ~printer:string_of_int (-30) result

let multiply__when_factors_are_negative =
  "multiply When factors are negative"
  >:: fun _ ->
    let result = multiply (-3) (-10) in
    (* then *)
    assert_equal ~printer:string_of_int 30 result

let multiply_mod__when_modulo_and_factors_are_positive =
  "multiply_mod When modulo and factors are positive"
  >:: fun _ ->
    (* when *)
    let result = multiply_mod 547 312 10000 in
    (* then *)
    assert_equal ~printer:string_of_int 664 result

let multiply_mod__when_modulo_is_positive_and_factor1_is_negative =
  "multiply_mod When modulo is positive and factor1 is negative"
  >:: fun _ ->
    (* when *)
    let result = multiply_mod (-547) 312 10000 in
    (* then *)
    assert_equal ~printer:string_of_int 9336 result

let multiply_mod__when_modulo_is_positive_and_factor2_is_negative =
  "multiply_mod When modulo is positive and factor2 is negative"
  >:: fun _ ->
    (* when *)
    let result = multiply_mod 547 (-312) 10000 in
    (* then *)
    assert_equal ~printer:string_of_int 9336 result

let multiply_mod__when_modulo_is_positive_and_factors_are_negative =
  "multiply_mod When modulo is positive and factors are negative"
  >:: fun _ ->
    (* when *)
    let result = multiply_mod (-547) (-312) 10000 in
    (* then *)
    assert_equal ~printer:string_of_int 664 result

let multiply_mod__when_modulo_is_negative__then_failure =
  "multiply_mod When modulo is negative Then Failure"
  >:: fun _ ->
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
      multiply__when_factors_are_negative; multiply_mod__when_modulo_and_factors_are_positive;
      multiply_mod__when_modulo_is_positive_and_factor1_is_negative;
      multiply_mod__when_modulo_is_positive_and_factor2_is_negative;
      multiply_mod__when_modulo_is_positive_and_factors_are_negative;
      multiply_mod__when_modulo_is_negative__then_failure ]

(* power_Test_list *)

let power__when_base_is_zero__then_returns_zero =
  "power When base is zero Then returns zero"
  >:: fun _ ->
    (* when *)
    let result = power 0 14 in
    (* then *)
    assert_equal ~printer:string_of_int 0 result

let power__when_exponent_is_zero__then_returns_one =
  "power When exponent is zero Then returns one"
  >:: fun _ ->
    (* when *)
    let result = power 14 0 in
    (* then *)
    assert_equal ~printer:string_of_int 1 result

let power__when_base_and_exponent_are_zero__then_failure =
  "power When base and exponent are zero Then Failure"
  >:: fun _ ->
    (* when *)
    let exec () = power 0 0 in
    (* then *)
    assert_raises (Failure "Not a number") exec

let power__when_base_and_exponent_are_positive =
  "power When base and exponent are positive"
  >:: fun _ ->
    (* when *)
    let result = power 3 10 in
    (* then *)
    assert_equal ~printer:string_of_int 59049 result

let power__when_base_is_negative_and_exponent_is_even =
  "power When base is negative and exponent is even"
  >:: fun _ ->
    (* when *)
    let result = power (-3) 10 in
    (* then *)
    assert_equal ~printer:string_of_int 59049 result

let power__when_base_is_negative_and_exponent_is_odd =
  "power When base is negative and exponent is odd"
  >:: fun _ ->
    (* when *)
    let result = power (-3) 9 in
    (* then *)
    assert_equal ~printer:string_of_int (-19683) result

let power__when_exponent_is_negative__then_failure =
  "power When exponent is negative Then Failure"
  >:: fun _ ->
    (* when *)
    let exec () = power 3 (-10) in
    (* then *)
    assert_raises (Failure "Negative exponent") exec

let power_mod__when_modulo_and_base_are_positive =
  "power_mod When modulo and base are positive"
  >:: fun _ ->
    (* when *)
    let result = power_mod 5 11 10000 in
    (* then *)
    assert_equal ~printer:string_of_int 8125 result

let power_mod__when_modulo_is_positive_and_base_is_negative =
  "power_mod When modulo is positive and base is negative"
  >:: fun _ ->
    (* when *)
    let result = power_mod (-5) 11 10000 in
    (* then *)
    assert_equal ~printer:string_of_int 1875 result

let power_mod__when_modulo_is_negative__then_failure =
  "power_mod When modulo is negative Then Failure"
  >:: fun _ ->
    (* when *)
    let exec () = power_mod 5 11 (-10000) in
    (* then *)
    assert_raises (Failure "Non-positive modulo") exec

let power_Test_list =
  test_list
    [ power__when_base_is_zero__then_returns_zero; power__when_exponent_is_zero__then_returns_one;
      power__when_base_and_exponent_are_zero__then_failure;
      power__when_base_and_exponent_are_positive; power__when_base_is_negative_and_exponent_is_even;
      power__when_base_is_negative_and_exponent_is_odd;
      power__when_exponent_is_negative__then_failure; power_mod__when_modulo_and_base_are_positive;
      power_mod__when_modulo_is_positive_and_base_is_negative;
      power_mod__when_modulo_is_negative__then_failure ]

(* maths_Test *)

let maths_Test =
  "Tests: Basic mathematics algorithms"
  >::: [gcd_Test_list; lcm_Test_list; multiply_Test_list; power_Test_list]

let _ = run_test_tt_main maths_Test
