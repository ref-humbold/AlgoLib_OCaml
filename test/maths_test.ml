open OUnit2
open Algolib.Maths

let test_gcdiv_when_numbers_are_composite _ =
  let number1 = 161 and number2 = 46 in
  let result = gcdiv number1 number2 in
  assert_equal 23 result

let test_gcd_when_numbers_are_prime _ =
  let number1 = 127 and number2 = 41 in
  let result = gcdiv number1 number2 in
  assert_equal 1 result

let test_gcd_when_numbers_are_mutually_prime _ =
  let number1 = 119 and number2 = 57 in
  let result = gcdiv number1 number2 in
  assert_equal 1 result

let test_gcd_when_one_of_numbers_is_multiple_of_another _ =
  let number1 = 272 and number2 = 34 in
  let result = gcdiv number1 number2 in
  assert_equal number2 result

let test_gcd_when_one_of_numbers_is_zero _ =
  let number1 = 96 and number2 = 0 in
  let result = gcdiv number1 number2 in
  assert_equal number1 result

let maths_test =
  "maths_test"
  >::: [ "test_gcdiv_when_numbers_are_composite" >:: test_gcdiv_when_numbers_are_composite;
         "test_gcd_when_numbers_are_prime" >:: test_gcd_when_numbers_are_prime;
         "test_gcd_when_numbers_are_mutually_prime" >:: test_gcd_when_numbers_are_mutually_prime;
         "test_gcd_when_one_of_numbers_is_multiple_of_another"
         >:: test_gcd_when_one_of_numbers_is_multiple_of_another;
         "test_gcd_when_one_of_numbers_is_zero" >:: test_gcd_when_one_of_numbers_is_zero ]

let _ = run_test_tt_main maths_test
