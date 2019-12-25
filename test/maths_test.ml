(* Tests: Basic mathematics algorithms. *)
open OUnit2
open Algolib.Maths

(* gcdiv *)

let gcdiv_When_numbers_are_composite_Then_returns_gcd =
  "Greatest common divisor of two composite numbers"
  >:: fun _ ->
    let number1 = 161 and number2 = 46 in
    let result = number1 **/ number2 in
    assert_equal ~printer:string_of_int 23 result

let gcdiv_When_numbers_are_prime_Then_returns_one =
  "Greatest common divisor of two prime numbers"
  >:: fun _ ->
    let number1 = 127 and number2 = 41 in
    let result = number1 **/ number2 in
    assert_equal ~printer:string_of_int 1 result

let gcdiv_When_numbers_are_mutually_prime_Then_returns_one =
  "Greatest common divisor of two mutually prime numbers"
  >:: fun _ ->
    let number1 = 119 and number2 = 57 in
    let result = number1 **/ number2 in
    assert_equal ~printer:string_of_int 1 result

let gcdiv_When_number1_is_multiple_of_number2_Then_returns_number2 =
  "Greatest common divisor of multiple"
  >:: fun _ ->
    let number1 = 272 and number2 = 34 in
    let result = number1 **/ number2 in
    assert_equal ~printer:string_of_int number2 result

let gcdiv_When_number2_is_zero_Then_returns_number1 =
  "Greatest common divisor with zero"
  >:: fun _ ->
    let number1 = 96 and number2 = 0 in
    let result = number1 **/ number2 in
    assert_equal ~printer:string_of_int number1 result

let gcdiv_Test =
  test_list
    [ gcdiv_When_numbers_are_composite_Then_returns_gcd;
      gcdiv_When_numbers_are_prime_Then_returns_one;
      gcdiv_When_numbers_are_mutually_prime_Then_returns_one;
      gcdiv_When_number1_is_multiple_of_number2_Then_returns_number2;
      gcdiv_When_number2_is_zero_Then_returns_number1 ]

(* lcmul *)

let lcmul_When_numbers_are_composite_Then_returns_lcm =
  "Lowest common multiple of two composite numbers"
  >:: fun _ ->
    let number1 = 161 and number2 = 46 in
    let result = number1 **^ number2 in
    assert_equal ~printer:string_of_int 322 result

let lcmul_When_numbers_are_prime_Then_returns_product =
  "Lowest common multiple of two prime numbers"
  >:: fun _ ->
    let number1 = 127 and number2 = 41 in
    let result = number1 **^ number2 in
    assert_equal ~printer:string_of_int 5207 result

let lcmul_When_numbers_are_mutually_prime_Then_returns_product =
  "Lowest common multiple of two mutually prime numbers"
  >:: fun _ ->
    let number1 = 119 and number2 = 57 in
    let result = number1 **^ number2 in
    assert_equal ~printer:string_of_int 6783 result

let lcmul_When_number1_is_multiple_of_number2_Then_returns_number1 =
  "Lowest common multiple with multiple"
  >:: fun _ ->
    let number1 = 272 and number2 = 34 in
    let result = number1 **^ number2 in
    assert_equal ~printer:string_of_int number1 result

let lcmul_When_number2_is_zero_Then_returns_zero =
  "Lowest common multiple with zero"
  >:: fun _ ->
    let number1 = 96 and number2 = 0 in
    let result = number1 **^ number2 in
    assert_equal ~printer:string_of_int 0 result

let lcmul_Test =
  test_list
    [ lcmul_When_numbers_are_composite_Then_returns_lcm;
      lcmul_When_numbers_are_prime_Then_returns_product;
      lcmul_When_numbers_are_mutually_prime_Then_returns_product;
      lcmul_When_number1_is_multiple_of_number2_Then_returns_number1;
      lcmul_When_number2_is_zero_Then_returns_zero ]

(* power_mod *)

let power_mod_When_base_is_zero_Then_returns_zero =
  "power_mod_When_base_is_zero"
  >:: fun _ ->
    let number1 = 0 and number2 = 14 in
    let result = power_mod number1 number2 in
    assert_equal ~printer:string_of_int 0 result

let power_mod_When_exponent_is_zero_Then_returns_one =
  "power_mod_When_exponent_is_zero"
  >:: fun _ ->
    let number1 = 14 and number2 = 0 in
    let result = power_mod number1 number2 in
    assert_equal ~printer:string_of_int 1 result

let power_mod_When_base_and_exponent_are_zero_Then_raises_failure =
  "power_mod_When_base_and_exponent_are_zero"
  >:: fun _ ->
    let number1 = 0 and number2 = 0 in
    let result () = power_mod number1 number2 in
    assert_raises (Failure "Not a number") result

let power_mod_When_base_and_exponent_are_positive =
  "power_mod_When_base_and_exponent_are_positive"
  >:: fun _ ->
    let number1 = 3 and number2 = 10 in
    let result = power_mod number1 number2 in
    assert_equal ~printer:string_of_int 59049 result

let power_mod_When_base_is_negative_and_exponent_is_even =
  "power_mod_When_base_is_negative_and_exponent_is_even"
  >:: fun _ ->
    let number1 = -3 and number2 = 10 in
    let result = power_mod number1 number2 in
    assert_equal ~printer:string_of_int 59049 result

let power_mod_When_base_is_negative_and_exponent_is_odd =
  "power_mod_When_base_is_negative_and_exponent_is_odd"
  >:: fun _ ->
    let number1 = -3 and number2 = 9 in
    let result = power_mod number1 number2 in
    assert_equal ~printer:string_of_int (-19683) result

let power_mod_When_exponent_is_negative_Then_raises_failure =
  "power_mod_When_exponent_is_negative"
  >:: fun _ ->
    let number1 = 3 and number2 = -10 in
    let result () = power_mod number1 number2 in
    assert_raises (Failure "Negative exponent") result

let power_mod_When_modulo_and_base_are_positive =
  "power_mod_When_modulo_and_base_are_positive"
  >:: fun _ ->
    let number1 = 5 and number2 = 11 and number3 = 10000 in
    let result = power_mod ~modulo:number3 number1 number2 in
    assert_equal ~printer:string_of_int 8125 result

let power_mod_When_modulo_is_positive_and_base_is_negative =
  "power_mod_When_modulo_is_positive_and_base_is_negative"
  >:: fun _ ->
    let number1 = -5 and number2 = 11 and number3 = 10000 in
    let result = power_mod ~modulo:number3 number1 number2 in
    assert_equal ~printer:string_of_int 1875 result

let power_mod_When_modulo_is_negative_Then_raises_failure =
  "power_mod_When_modulo_is_negative"
  >:: fun _ ->
    let number1 = 5 and number2 = 11 and number3 = -10000 in
    let result () = power_mod ~modulo:number3 number1 number2 in
    assert_raises (Failure "Negative modulo") result

let power_mod_Test =
  test_list
    [ power_mod_When_base_is_zero_Then_returns_zero;
      power_mod_When_exponent_is_zero_Then_returns_one;
      power_mod_When_base_and_exponent_are_zero_Then_raises_failure;
      power_mod_When_base_and_exponent_are_positive;
      power_mod_When_base_is_negative_and_exponent_is_even;
      power_mod_When_base_is_negative_and_exponent_is_odd;
      power_mod_When_exponent_is_negative_Then_raises_failure;
      power_mod_When_modulo_and_base_are_positive;
      power_mod_When_modulo_is_positive_and_base_is_negative;
      power_mod_When_modulo_is_negative_Then_raises_failure ]

(* mult_mod *)

let mult_mod_When_factor1_is_zero_Then_returns_zero =
  "mult_mod_When_factor1_is_zero"
  >:: fun _ ->
    let number1 = 0 and number2 = 14 in
    let result = mult_mod number1 number2 in
    assert_equal ~printer:string_of_int 0 result

let mult_mod_When_factor2_is_zero_Then_returns_zero =
  "mult_mod_When_factor2_is_zero"
  >:: fun _ ->
    let number1 = 14 and number2 = 0 in
    let result = mult_mod number1 number2 in
    assert_equal ~printer:string_of_int 0 result

let mult_mod_When_factors_are_zero_Then_returns_zero =
  "mult_mod_When_factors_are_zero"
  >:: fun _ ->
    let number1 = 0 and number2 = 0 in
    let result = mult_mod number1 number2 in
    assert_equal ~printer:string_of_int 0 result

let mult_mod_When_factor1_is_negative_and_factor2_is_positive =
  "mult_mod_When_factor1_is_negative_and_factor2_is_positive"
  >:: fun _ ->
    let number1 = -3 and number2 = 10 in
    let result = mult_mod number1 number2 in
    assert_equal ~printer:string_of_int (-30) result

let mult_mod_When_factor1_is_positive_and_factor2_is_negative =
  "mult_mod_When_factor1_is_positive_and_factor2_is_negative"
  >:: fun _ ->
    let number1 = 3 and number2 = -10 in
    let result = mult_mod number1 number2 in
    assert_equal ~printer:string_of_int (-30) result

let mult_mod_When_factors_are_negative =
  "mult_mod_When_factors_are_negative"
  >:: fun _ ->
    let number1 = -3 and number2 = -10 in
    let result = mult_mod number1 number2 in
    assert_equal ~printer:string_of_int 30 result

let mult_mod_When_modulo_and_factors_are_positive =
  "mult_mod_When_modulo_and_factors_are_positive"
  >:: fun _ ->
    let number1 = 547 and number2 = 312 and number3 = 10000 in
    let result = mult_mod ~modulo:number3 number1 number2 in
    assert_equal ~printer:string_of_int 664 result

let mult_mod_When_modulo_is_positive_and_factor1_is_negative =
  "mult_mod_When_modulo_is_positive_and_factor1_is_negative"
  >:: fun _ ->
    let number1 = -547 and number2 = 312 and number3 = 10000 in
    let result = mult_mod ~modulo:number3 number1 number2 in
    assert_equal ~printer:string_of_int 9336 result

let mult_mod_When_modulo_is_positive_and_factor2_is_negative =
  "mult_mod_When_modulo_is_positive_and_factor2_is_negative"
  >:: fun _ ->
    let number1 = 547 and number2 = -312 and number3 = 10000 in
    let result = mult_mod ~modulo:number3 number1 number2 in
    assert_equal ~printer:string_of_int 9336 result

let mult_mod_When_modulo_is_positive_and_factors_are_negative =
  "mult_mod_When_modulo_is_positive_and_factors_are_negative"
  >:: fun _ ->
    let number1 = -547 and number2 = -312 and number3 = 10000 in
    let result = mult_mod ~modulo:number3 number1 number2 in
    assert_equal ~printer:string_of_int 664 result

let mult_mod_When_modulo_is_negative_Then_raises_failure =
  "mult_mod_When_modulo_is_negative"
  >:: fun _ ->
    let number1 = 547 and number2 = 312 and number3 = -10000 in
    let result () = mult_mod ~modulo:number3 number1 number2 in
    assert_raises (Failure "Negative modulo") result

let mult_mod_Test =
  test_list
    [ mult_mod_When_factor1_is_zero_Then_returns_zero;
      mult_mod_When_factor2_is_zero_Then_returns_zero;
      mult_mod_When_factors_are_zero_Then_returns_zero;
      mult_mod_When_factor1_is_negative_and_factor2_is_positive;
      mult_mod_When_factor1_is_positive_and_factor2_is_negative; mult_mod_When_factors_are_negative;
      mult_mod_When_modulo_and_factors_are_positive;
      mult_mod_When_modulo_is_positive_and_factor1_is_negative;
      mult_mod_When_modulo_is_positive_and_factor2_is_negative;
      mult_mod_When_modulo_is_positive_and_factors_are_negative;
      mult_mod_When_modulo_is_negative_Then_raises_failure ]

(* maths test *)

let maths_Test =
  "Tests: Basic mathematics algorithms" >::: [gcdiv_Test; lcmul_Test; power_mod_Test; mult_mod_Test]

let _ = run_test_tt_main maths_Test
