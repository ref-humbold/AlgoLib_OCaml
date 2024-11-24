(* Tests: Algorithms for testing prime numbers. *)
open OUnit2
open Algolib.Primes_testing
open TestUtils

(* test_primes_fermat_Test_list *)

let test_primes_fermat__when_zero__then_false =
  "test_primes_fermat__when_zero__then_false" >:: fun _ ->
    (* when *)
    let result = test_primes_fermat 0 in
    (* then *)
    Assert.Bool.assert_false result

let test_primes_fermat__when_one__then_false =
  "test_primes_fermat__when_one__then_false" >:: fun _ ->
    (* when *)
    let result = test_primes_fermat 1 in
    (* then *)
    Assert.Bool.assert_false result

let test_primes_fermat__when_two__then_true =
  "test_primes_fermat__when_two__then_true" >:: fun _ ->
    (* when *)
    let result = test_primes_fermat 2 in
    (* then *)
    Assert.Bool.assert_true result

let test_primes_fermat__when_prime_1__then_true =
  "test_primes_fermat__when_prime_1__then_true" >:: fun _ ->
    (* when *)
    let result = test_primes_fermat 1013 in
    (* then *)
    Assert.Bool.assert_true result

let test_primes_fermat__when_prime_2__then_true =
  "test_primes_fermat__when_prime_2__then_true" >:: fun _ ->
    (* when *)
    let result = test_primes_fermat 2131 in
    (* then *)
    Assert.Bool.assert_true result

let test_primes_fermat__when_prime_3__then_true =
  "test_primes_fermat__when_prime_3__then_true" >:: fun _ ->
    (* when *)
    let result = test_primes_fermat 6199 in
    (* then *)
    Assert.Bool.assert_true result

let test_primes_fermat__when_composite_1__then_false =
  "test_primes_fermat__when_composite_1__then_false" >:: fun _ ->
    (* when *)
    (* 1001 = 7 * 11 * 13 *)
    let result = test_primes_fermat 1001 in
    (* then *)
    Assert.Bool.assert_false result

let test_primes_fermat__when_composite_2__then_false =
  "test_primes_fermat__when_composite_2__then_false" >:: fun _ ->
    (* when *)
    (* 41041 = 7 * 11 * 13 * 41 *)
    let result = test_primes_fermat 41041 in
    (* then *)
    Assert.Bool.assert_false result

let test_primes_fermat__when_composite_3__then_false =
  "test_primes_fermat__when_composite_3__then_false" >:: fun _ ->
    (* when *)
    (* 73627 = 17 * 61 * 71 *)
    let result = test_primes_fermat 73627 in
    (* then *)
    Assert.Bool.assert_false result

let test_primes_fermat__when_composite_square_of_prime__then_false =
  "test_primes_fermat__when_composite_square_of_prime__then_false" >:: fun _ ->
    (* when *)
    (* 3481 = 59 ^ 2 *)
    let result = test_primes_fermat 3481 in
    (* then *)
    Assert.Bool.assert_false result

let test_primes_fermat_Test_list =
  test_list
    [ test_primes_fermat__when_zero__then_false;
      test_primes_fermat__when_one__then_false;
      test_primes_fermat__when_two__then_true;
      test_primes_fermat__when_prime_1__then_true;
      test_primes_fermat__when_prime_2__then_true;
      test_primes_fermat__when_prime_3__then_true;
      test_primes_fermat__when_composite_1__then_false;
      test_primes_fermat__when_composite_2__then_false;
      test_primes_fermat__when_composite_3__then_false;
      test_primes_fermat__when_composite_square_of_prime__then_false ]

(* test_primes_miller_Test_list *)

let test_primes_miller__when_zero__then_false =
  "test_primes_miller__when_zero__then_false" >:: fun _ ->
    (* when *)
    let result = test_primes_miller 0 in
    (* then *)
    Assert.Bool.assert_false result

let test_primes_miller__when_one__then_false =
  "test_primes_miller__when_one__then_false" >:: fun _ ->
    (* when *)
    let result = test_primes_miller 1 in
    (* then *)
    Assert.Bool.assert_false result

let test_primes_miller__when_two__then_true =
  "test_primes_miller__when_two__then_true" >:: fun _ ->
    (* when *)
    let result = test_primes_miller 2 in
    (* then *)
    Assert.Bool.assert_true result

let test_primes_miller__when_prime_1__then_true =
  "test_primes_miller__when_prime_1__then_true" >:: fun _ ->
    (* when *)
    let result = test_primes_miller 1013 in
    (* then *)
    Assert.Bool.assert_true result

let test_primes_miller__when_prime_2__then_true =
  "test_primes_miller__when_prime_2__then_true" >:: fun _ ->
    (* when *)
    let result = test_primes_miller 2131 in
    (* then *)
    Assert.Bool.assert_true result

let test_primes_miller__when_prime_3__then_true =
  "test_primes_miller__when_prime_3__then_true" >:: fun _ ->
    (* when *)
    let result = test_primes_miller 6199 in
    (* then *)
    Assert.Bool.assert_true result

let test_primes_miller__when_composite_1__then_false =
  "test_primes_miller__when_composite_1__then_false" >:: fun _ ->
    (* when *)
    let result = test_primes_miller 1001 in
    (* then *)
    Assert.Bool.assert_false result

let test_primes_miller__when_composite_2__then_false =
  "test_primes_miller__when_composite_2__then_false" >:: fun _ ->
    (* when *)
    let result = test_primes_miller 41041 in
    (* then *)
    Assert.Bool.assert_false result

let test_primes_miller__when_composite_3__then_false =
  "test_primes_miller__when_composite_3__then_false" >:: fun _ ->
    (* when *)
    let result = test_primes_miller 73627 in
    (* then *)
    Assert.Bool.assert_false result

let test_primes_miller__when_composite_square_of_prime__then_false =
  "test_primes_miller__when_composite_square_of_prime__then_false" >:: fun _ ->
    (* when *)
    (* 3481 = 59 ^ 2 *)
    let result = test_primes_miller 3481 in
    (* then *)
    Assert.Bool.assert_false result

let test_primes_miller_Test_list =
  test_list
    [ test_primes_miller__when_zero__then_false;
      test_primes_miller__when_one__then_false;
      test_primes_miller__when_two__then_true;
      test_primes_miller__when_prime_1__then_true;
      test_primes_miller__when_prime_2__then_true;
      test_primes_miller__when_prime_3__then_true;
      test_primes_miller__when_composite_1__then_false;
      test_primes_miller__when_composite_2__then_false;
      test_primes_miller__when_composite_3__then_false;
      test_primes_miller__when_composite_square_of_prime__then_false ]

(* primes_testing_Test *)

let primes_testing_Test =
  "Tests: Algorithms for testing prime numbers"
  >::: [test_primes_fermat_Test_list; test_primes_miller_Test_list]

let _ = run_test_tt_main primes_testing_Test
