open OUnit2
open Algolib.Maths

(* gcdiv *)

let test_gcdiv_when_numbers_are_composite =
  "test_gcdiv_when_numbers_are_composite"
  >:: fun _ ->
    let number1 = 161 and number2 = 46 in
    let result = number1 **/ number2 in
    assert_equal ~printer:string_of_int 23 result

let test_gcdiv_when_numbers_are_prime =
  "test_gcdiv_when_numbers_are_prime"
  >:: fun _ ->
    let number1 = 127 and number2 = 41 in
    let result = number1 **/ number2 in
    assert_equal ~printer:string_of_int 1 result

let test_gcdiv_when_numbers_are_mutually_prime =
  "test_gcdiv_when_numbers_are_mutually_prime"
  >:: fun _ ->
    let number1 = 119 and number2 = 57 in
    let result = number1 **/ number2 in
    assert_equal ~printer:string_of_int 1 result

let test_gcdiv_when_one_of_numbers_is_multiple_of_another =
  "test_gcdiv_when_one_of_numbers_is_multiple_of_another"
  >:: fun _ ->
    let number1 = 272 and number2 = 34 in
    let result = number1 **/ number2 in
    assert_equal ~printer:string_of_int number2 result

let test_gcdiv_when_one_of_numbers_is_zero =
  "test_gcdiv_when_one_of_numbers_is_zero"
  >:: fun _ ->
    let number1 = 96 and number2 = 0 in
    let result = number1 **/ number2 in
    assert_equal ~printer:string_of_int number1 result

(* lcmul *)

let test_lcmul_when_numbers_are_composite =
  "test_lcmul_when_numbers_are_composite"
  >:: fun _ ->
    let number1 = 161 and number2 = 46 in
    let result = number1 **^ number2 in
    assert_equal ~printer:string_of_int 322 result

let test_lcmul_when_numbers_are_prime =
  "test_lcmul_when_numbers_are_prime"
  >:: fun _ ->
    let number1 = 127 and number2 = 41 in
    let result = number1 **^ number2 in
    assert_equal ~printer:string_of_int 5207 result

let test_lcmul_when_numbers_are_mutually_prime =
  "test_lcmul_when_numbers_are_mutually_prime"
  >:: fun _ ->
    let number1 = 119 and number2 = 57 in
    let result = number1 **^ number2 in
    assert_equal ~printer:string_of_int 6783 result

let test_lcmul_when_one_of_numbers_is_multiple_of_another =
  "test_lcmul_when_one_of_numbers_is_multiple_of_another"
  >:: fun _ ->
    let number1 = 272 and number2 = 34 in
    let result = number1 **^ number2 in
    assert_equal ~printer:string_of_int number1 result

let test_lcmul_when_one_of_numbers_is_zero =
  "test_lcmul_when_one_of_numbers_is_zero"
  >:: fun _ ->
    let number1 = 96 and number2 = 0 in
    let result = number1 **^ number2 in
    assert_equal ~printer:string_of_int number2 result

(* power_mod *)

let test_power_mod_when_base_is_zero =
  "test_power_mod_when_base_is_zero"
  >:: fun _ ->
    let number1 = 0 and number2 = 14 in
    let result = power_mod number1 number2 in
    assert_equal ~printer:string_of_int number1 result

let test_power_mod_when_exponent_is_zero =
  "test_power_mod_when_exponent_is_zero"
  >:: fun _ ->
    let number1 = 14 and number2 = 0 in
    let result = power_mod number1 number2 in
    assert_equal ~printer:string_of_int 1 result

let test_power_mod_when_base_and_exponent_are_zero =
  "test_power_mod_when_base_and_exponent_are_zero"
  >:: fun _ ->
    let number1 = 0 and number2 = 0 in
    let result () = power_mod number1 number2 in
    assert_raises (Failure "Not a number") result

let test_power_mod_when_base_and_exponent_are_positive =
  "test_power_mod_when_base_and_exponent_are_positive"
  >:: fun _ ->
    let number1 = 3 and number2 = 10 in
    let result = power_mod number1 number2 in
    assert_equal ~printer:string_of_int 59049 result

let test_power_mod_when_base_is_negative_and_exponent_is_even =
  "test_power_mod_when_base_is_negative_and_exponent_is_even"
  >:: fun _ ->
    let number1 = -3 and number2 = 10 in
    let result = power_mod number1 number2 in
    assert_equal ~printer:string_of_int 59049 result

let test_power_mod_when_base_is_negative_and_exponent_is_odd =
  "test_power_mod_when_base_is_negative_and_exponent_is_odd"
  >:: fun _ ->
    let number1 = -3 and number2 = 9 in
    let result = power_mod number1 number2 in
    assert_equal ~printer:string_of_int (-19683) result

let test_power_mod_when_exponent_is_negative =
  "test_power_mod_when_exponent_is_negative"
  >:: fun _ ->
    let number1 = 3 and number2 = -10 in
    let result () = power_mod number1 number2 in
    assert_raises (Failure "Negative exponent") result

let test_power_mod_when_modulo_and_base_are_positive =
  "test_power_mod_when_modulo_and_base_are_positive"
  >:: fun _ ->
    let number1 = 5 and number2 = 11 and number3 = 10000 in
    let result = power_mod ~modulo:number3 number1 number2 in
    assert_equal ~printer:string_of_int 8125 result

let test_power_mod_when_modulo_is_positive_and_base_is_negative =
  "test_power_mod_when_modulo_is_positive_and_base_is_negative"
  >:: fun _ ->
    let number1 = -5 and number2 = 11 and number3 = 10000 in
    let result = power_mod ~modulo:number3 number1 number2 in
    assert_equal ~printer:string_of_int 1875 result

let test_power_mod_when_modulo_is_negative =
  "test_power_mod_when_modulo_is_negative"
  >:: fun _ ->
    let number1 = 5 and number2 = 11 and number3 = -10000 in
    let result () = power_mod ~modulo:number3 number1 number2 in
    assert_raises (Failure "Negative modulo") result

(* mult_mod *)

let test_mult_mod_when_factor1_is_zero =
  "test_mult_mod_when_factor1_is_zero"
  >:: fun _ ->
    let number1 = 0 and number2 = 14 in
    let result = mult_mod number1 number2 in
    assert_equal ~printer:string_of_int number1 result

let test_mult_mod_when_factor2_is_zero =
  "test_mult_mod_when_factor2_is_zero"
  >:: fun _ ->
    let number1 = 14 and number2 = 0 in
    let result = mult_mod number1 number2 in
    assert_equal ~printer:string_of_int number2 result

let test_mult_mod_when_factors_are_zero =
  "test_mult_mod_when_factors_are_zero"
  >:: fun _ ->
    let number1 = 0 and number2 = 0 in
    let result = mult_mod number1 number2 in
    assert_equal ~printer:string_of_int number1 result

let test_mult_mod_when_factor1_is_negative_and_factor2_is_positive =
  "test_mult_mod_when_factor1_is_negative_and_factor2_is_positive"
  >:: fun _ ->
    let number1 = -3 and number2 = 10 in
    let result = mult_mod number1 number2 in
    assert_equal ~printer:string_of_int (-30) result

let test_mult_mod_when_factor1_is_positive_and_factor2_is_negative =
  "test_mult_mod_when_factor1_is_positive_and_factor2_is_negative"
  >:: fun _ ->
    let number1 = 3 and number2 = -10 in
    let result = mult_mod number1 number2 in
    assert_equal ~printer:string_of_int (-30) result

let test_mult_mod_when_factors_are_negative =
  "test_mult_mod_when_factors_are_negative"
  >:: fun _ ->
    let number1 = -3 and number2 = -10 in
    let result = mult_mod number1 number2 in
    assert_equal ~printer:string_of_int 30 result

let test_mult_mod_when_modulo_and_factors_are_positive =
  "test_mult_mod_when_modulo_and_factors_are_positive"
  >:: fun _ ->
    let number1 = 547 and number2 = 312 and number3 = 10000 in
    let result = mult_mod ~modulo:number3 number1 number2 in
    assert_equal ~printer:string_of_int 664 result

let test_mult_mod_when_modulo_is_positive_and_factor1_is_negative =
  "test_mult_mod_when_modulo_is_positive_and_factor1_is_negative"
  >:: fun _ ->
    let number1 = -547 and number2 = 312 and number3 = 10000 in
    let result = mult_mod ~modulo:number3 number1 number2 in
    assert_equal ~printer:string_of_int 9336 result

let test_mult_mod_when_modulo_is_positive_and_factor2_is_negative =
  "test_mult_mod_when_modulo_is_positive_and_factor2_is_negative"
  >:: fun _ ->
    let number1 = 547 and number2 = -312 and number3 = 10000 in
    let result = mult_mod ~modulo:number3 number1 number2 in
    assert_equal ~printer:string_of_int 9336 result

let test_mult_mod_when_modulo_is_positive_and_factors_are_negative =
  "test_mult_mod_when_modulo_is_positive_and_factors_are_negative"
  >:: fun _ ->
    let number1 = -547 and number2 = -312 and number3 = 10000 in
    let result = mult_mod ~modulo:number3 number1 number2 in
    assert_equal ~printer:string_of_int 664 result

let test_mult_mod_when_modulo_is_negative =
  "test_mult_mod_when_modulo_is_negative"
  >:: fun _ ->
    let number1 = 547 and number2 = 312 and number3 = -10000 in
    let result () = mult_mod ~modulo:number3 number1 number2 in
    assert_raises (Failure "Negative modulo") result

(* maths test *)

let maths_test =
  "maths_test"
  >::: [ test_gcdiv_when_numbers_are_composite; test_gcdiv_when_numbers_are_prime;
         test_gcdiv_when_numbers_are_mutually_prime;
         test_gcdiv_when_one_of_numbers_is_multiple_of_another;
         test_gcdiv_when_one_of_numbers_is_zero; test_lcmul_when_numbers_are_composite;
         test_lcmul_when_numbers_are_prime; test_lcmul_when_numbers_are_mutually_prime;
         test_lcmul_when_one_of_numbers_is_multiple_of_another;
         test_lcmul_when_one_of_numbers_is_zero; test_power_mod_when_base_is_zero;
         test_power_mod_when_exponent_is_zero; test_power_mod_when_base_and_exponent_are_zero;
         test_power_mod_when_base_and_exponent_are_positive;
         test_power_mod_when_base_is_negative_and_exponent_is_even;
         test_power_mod_when_base_is_negative_and_exponent_is_odd;
         test_power_mod_when_exponent_is_negative;
         test_power_mod_when_modulo_and_base_are_positive;
         test_power_mod_when_modulo_is_positive_and_base_is_negative;
         test_power_mod_when_modulo_is_negative; test_mult_mod_when_factor1_is_zero;
         test_mult_mod_when_factor2_is_zero; test_mult_mod_when_factors_are_zero;
         test_mult_mod_when_factor1_is_negative_and_factor2_is_positive;
         test_mult_mod_when_factor1_is_positive_and_factor2_is_negative;
         test_mult_mod_when_factors_are_negative;
         test_mult_mod_when_modulo_and_factors_are_positive;
         test_mult_mod_when_modulo_is_positive_and_factor1_is_negative;
         test_mult_mod_when_modulo_is_positive_and_factor2_is_negative;
         test_mult_mod_when_modulo_is_positive_and_factors_are_negative;
         test_mult_mod_when_modulo_is_negative ]

let _ = run_test_tt_main maths_test
