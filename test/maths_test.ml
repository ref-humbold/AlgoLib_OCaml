open OUnit2
open Algolib.Maths

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

let maths_test =
  "maths_test"
  >::: [ test_gcdiv_when_numbers_are_composite; test_gcdiv_when_numbers_are_prime;
         test_gcdiv_when_numbers_are_mutually_prime;
         test_gcdiv_when_one_of_numbers_is_multiple_of_another;
         test_gcdiv_when_one_of_numbers_is_zero; test_lcmul_when_numbers_are_composite;
         test_lcmul_when_numbers_are_prime; test_lcmul_when_numbers_are_mutually_prime;
         test_lcmul_when_one_of_numbers_is_multiple_of_another;
         test_lcmul_when_one_of_numbers_is_zero ]

let _ = run_test_tt_main maths_test
