(* Tests: Basic mathematics algorithms *)
open OUnit2
open Algolib.Maths

(* gcd_Test_list *)

let gcd__when_numbers_are_composite__then_returns_gcd =
  "gcd When numbers are composite Then returns gcd"
  >:: fun _ ->
  let number1 = 161 and number2 = 46 in
  let result = number1 **/ number2 in
  assert_equal ~printer:string_of_int 23 result

let gcd__when_numbers_are_prime__then_returns_one =
  "gcd When numbers are prime Then returns one"
  >:: fun _ ->
  let number1 = 127 and number2 = 41 in
  let result = number1 **/ number2 in
  assert_equal ~printer:string_of_int 1 result

let gcd__when_numbers_are_mutually_prime__then_returns_one =
  "gcd When numbers are mutually prime Then returns one"
  >:: fun _ ->
  let number1 = 119 and number2 = 57 in
  let result = number1 **/ number2 in
  assert_equal ~printer:string_of_int 1 result

let gcd__when_number1_is_multiple_of_number2__then_returns_number2 =
  "gcd When number1 is multiple of number2 Then returns number2"
  >:: fun _ ->
  let number1 = 272 and number2 = 34 in
  let result = number1 **/ number2 in
  assert_equal ~printer:string_of_int number2 result

let gcd__when_number2_is_zero__then_returns_number1 =
  "gcd When number2 is zero Then returns number1"
  >:: fun _ ->
  let number1 = 96 and number2 = 0 in
  let result = number1 **/ number2 in
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
  let number1 = 161 and number2 = 46 in
  let result = number1 **^ number2 in
  assert_equal ~printer:string_of_int 322 result

let lcm__when_numbers_are_prime__then_returns_product =
  "lcm When numbers are prime Then returns product"
  >:: fun _ ->
  let number1 = 127 and number2 = 41 in
  let result = number1 **^ number2 in
  assert_equal ~printer:string_of_int 5207 result

let lcm__when_numbers_are_mutually_prime__then_returns_product =
  "lcm When numbers are mutually prime Then returns product"
  >:: fun _ ->
  let number1 = 119 and number2 = 57 in
  let result = number1 **^ number2 in
  assert_equal ~printer:string_of_int 6783 result

let lcm__when_number1_is_multiple_of_number2__then_returns_number1 =
  "lcm When number1 is multiple of number2 Then returns number1"
  >:: fun _ ->
  let number1 = 272 and number2 = 34 in
  let result = number1 **^ number2 in
  assert_equal ~printer:string_of_int number1 result

let lcm__when_number2_is_zero__then_returns_zero =
  "lcm When number2 is zero Then returns zero"
  >:: fun _ ->
  let number1 = 96 and number2 = 0 in
  let result = number1 **^ number2 in
  assert_equal ~printer:string_of_int 0 result

let lcm_Test_list =
  test_list
    [ lcm__when_numbers_are_composite__then_returns_lcm;
      lcm__when_numbers_are_prime__then_returns_product;
      lcm__when_numbers_are_mutually_prime__then_returns_product;
      lcm__when_number1_is_multiple_of_number2__then_returns_number1;
      lcm__when_number2_is_zero__then_returns_zero ]

(* power_mod_Test_list *)

let power_mod__when_base_is_zero__then_returns_zero =
  "power_mod When base is zero Then returns zero"
  >:: fun _ ->
  let number1 = 0 and number2 = 14 in
  let result = power_mod number1 number2 in
  assert_equal ~printer:string_of_int 0 result

let power_mod__when_exponent_is_zero__then_returns_one =
  "power_mod When exponent is zero Then returns one"
  >:: fun _ ->
  let number1 = 14 and number2 = 0 in
  let result = power_mod number1 number2 in
  assert_equal ~printer:string_of_int 1 result

let power_mod__when_base_and_exponent_are_zero__then_raises_failure =
  "power_mod When base and exponent are zero Then raises failure"
  >:: fun _ ->
  let number1 = 0 and number2 = 0 in
  let result () = power_mod number1 number2 in
  assert_raises (Failure "Not a number") result

let power_mod__when_base_and_exponent_are_positive =
  "power_mod When base and exponent are positive"
  >:: fun _ ->
  let number1 = 3 and number2 = 10 in
  let result = power_mod number1 number2 in
  assert_equal ~printer:string_of_int 59049 result

let power_mod__when_base_is_negative_and_exponent_is_even =
  "power_mod When base is negative and exponent is even"
  >:: fun _ ->
  let number1 = -3 and number2 = 10 in
  let result = power_mod number1 number2 in
  assert_equal ~printer:string_of_int 59049 result

let power_mod__when_base_is_negative_and_exponent_is_odd =
  "power_mod When base is negative and exponent is odd"
  >:: fun _ ->
  let number1 = -3 and number2 = 9 in
  let result = power_mod number1 number2 in
  assert_equal ~printer:string_of_int (-19683) result

let power_mod__when_exponent_is_negative__then_raises_failure =
  "power_mod When exponent is negative Then raises failure"
  >:: fun _ ->
  let number1 = 3 and number2 = -10 in
  let result () = power_mod number1 number2 in
  assert_raises (Failure "Negative exponent") result

let power_mod__when_modulo_and_base_are_positive =
  "power_mod When modulo and base are positive"
  >:: fun _ ->
  let number1 = 5 and number2 = 11 and number3 = 10000 in
  let result = power_mod ~modulo:number3 number1 number2 in
  assert_equal ~printer:string_of_int 8125 result

let power_mod__when_modulo_is_positive_and_base_is_negative =
  "power_mod When modulo is positive and base is negative"
  >:: fun _ ->
  let number1 = -5 and number2 = 11 and number3 = 10000 in
  let result = power_mod ~modulo:number3 number1 number2 in
  assert_equal ~printer:string_of_int 1875 result

let power_mod__when_modulo_is_negative__then_raises_failure =
  "power_mod When modulo is negative Then raises failure"
  >:: fun _ ->
  let number1 = 5 and number2 = 11 and number3 = -10000 in
  let result () = power_mod ~modulo:number3 number1 number2 in
  assert_raises (Failure "Negative modulo") result

let power_mod_Test_list =
  test_list
    [ power_mod__when_base_is_zero__then_returns_zero;
      power_mod__when_exponent_is_zero__then_returns_one;
      power_mod__when_base_and_exponent_are_zero__then_raises_failure;
      power_mod__when_base_and_exponent_are_positive;
      power_mod__when_base_is_negative_and_exponent_is_even;
      power_mod__when_base_is_negative_and_exponent_is_odd;
      power_mod__when_exponent_is_negative__then_raises_failure;
      power_mod__when_modulo_and_base_are_positive;
      power_mod__when_modulo_is_positive_and_base_is_negative;
      power_mod__when_modulo_is_negative__then_raises_failure ]

(* multiply_mod_Test_list *)

let multiply_mod__when_factor1_is_zero__then_returns_zero =
  "multiply_mod When factor1 is zero Then returns zero"
  >:: fun _ ->
  let number1 = 0 and number2 = 14 in
  let result = multiply_mod number1 number2 in
  assert_equal ~printer:string_of_int 0 result

let multiply_mod__when_factor2_is_zero__then_returns_zero =
  "multiply_mod When factor2 is zero Then returns zero"
  >:: fun _ ->
  let number1 = 14 and number2 = 0 in
  let result = multiply_mod number1 number2 in
  assert_equal ~printer:string_of_int 0 result

let multiply_mod__when_factors_are_zero__then_returns_zero =
  "multiply_mod When factors are zero Then returns zero"
  >:: fun _ ->
  let number1 = 0 and number2 = 0 in
  let result = multiply_mod number1 number2 in
  assert_equal ~printer:string_of_int 0 result

let multiply_mod__when_factor1_is_negative_and_factor2_is_positive =
  "multiply_mod When factor1 is negative and factor2 is positive"
  >:: fun _ ->
  let number1 = -3 and number2 = 10 in
  let result = multiply_mod number1 number2 in
  assert_equal ~printer:string_of_int (-30) result

let multiply_mod__when_factor1_is_positive_and_factor2_is_negative =
  "multiply_mod When factor1 is positive and factor2 is negative"
  >:: fun _ ->
  let number1 = 3 and number2 = -10 in
  let result = multiply_mod number1 number2 in
  assert_equal ~printer:string_of_int (-30) result

let multiply_mod__when_factors_are_negative =
  "multiply_mod When factors are negative"
  >:: fun _ ->
  let number1 = -3 and number2 = -10 in
  let result = multiply_mod number1 number2 in
  assert_equal ~printer:string_of_int 30 result

let multiply_mod__when_modulo_and_factors_are_positive =
  "multiply_mod When modulo and factors are positive"
  >:: fun _ ->
  let number1 = 547 and number2 = 312 and number3 = 10000 in
  let result = multiply_mod ~modulo:number3 number1 number2 in
  assert_equal ~printer:string_of_int 664 result

let multiply_mod__when_modulo_is_positive_and_factor1_is_negative =
  "multiply_mod When modulo is positive and factor1 is negative"
  >:: fun _ ->
  let number1 = -547 and number2 = 312 and number3 = 10000 in
  let result = multiply_mod ~modulo:number3 number1 number2 in
  assert_equal ~printer:string_of_int 9336 result

let multiply_mod__when_modulo_is_positive_and_factor2_is_negative =
  "multiply_mod When modulo is positive and factor2 is negative"
  >:: fun _ ->
  let number1 = 547 and number2 = -312 and number3 = 10000 in
  let result = multiply_mod ~modulo:number3 number1 number2 in
  assert_equal ~printer:string_of_int 9336 result

let multiply_mod__when_modulo_is_positive_and_factors_are_negative =
  "multiply_mod When modulo is positive and factors are negative"
  >:: fun _ ->
  let number1 = -547 and number2 = -312 and number3 = 10000 in
  let result = multiply_mod ~modulo:number3 number1 number2 in
  assert_equal ~printer:string_of_int 664 result

let multiply_mod__when_modulo_is_negative__then_raises_failure =
  "multiply_mod When modulo is negative Then raises failure"
  >:: fun _ ->
  let number1 = 547 and number2 = 312 and number3 = -10000 in
  let result () = multiply_mod ~modulo:number3 number1 number2 in
  assert_raises (Failure "Negative modulo") result

let multiply_mod_Test_list =
  test_list
    [ multiply_mod__when_factor1_is_zero__then_returns_zero;
      multiply_mod__when_factor2_is_zero__then_returns_zero;
      multiply_mod__when_factors_are_zero__then_returns_zero;
      multiply_mod__when_factor1_is_negative_and_factor2_is_positive;
      multiply_mod__when_factor1_is_positive_and_factor2_is_negative;
      multiply_mod__when_factors_are_negative; multiply_mod__when_modulo_and_factors_are_positive;
      multiply_mod__when_modulo_is_positive_and_factor1_is_negative;
      multiply_mod__when_modulo_is_positive_and_factor2_is_negative;
      multiply_mod__when_modulo_is_positive_and_factors_are_negative;
      multiply_mod__when_modulo_is_negative__then_raises_failure ]

(* maths_Test *)

let maths_Test =
  "Tests: Basic mathematics algorithms"
  >::: [gcd_Test_list; lcm_Test_list; power_mod_Test_list; multiply_mod_Test_list]

let _ = run_test_tt_main maths_Test
