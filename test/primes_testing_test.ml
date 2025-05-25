(* Tests: Algorithms for testing prime numbers. *)
open OUnit2
open OAssert
open Algolib.Maths.Primes_testing

(* 1001 = 7 * 11 * 13 ; 3481 = 59 ^ 2 ; 41041 = 7 * 11 * 13 * 41 ; 73627 = 17 * 61 * 71 *)
let params_not_primes = [0; 1; 77; 1001; 3481; 41041; 73627]

let params_primes = [2; 107; 1013; 2131; 6199]

(* test_primes_fermat_Test_list *)

let test_prime_fermat__when_not_prime__then_false param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ param in
  label >:: fun _ ->
    (* when *)
    let result = test_prime_fermat param in
    (* then *)
    assert_that result Is.false_

let test_prime_fermat__when_prime__then_false param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ param in
  label >:: fun _ ->
    (* when *)
    let result = test_prime_fermat param in
    (* then *)
    assert_that result Is.true_

let test_primes_fermat_Test_list =
  test_list
  @@ List.concat
    [ List.map (fun p -> test_prime_fermat__when_not_prime__then_false p) params_not_primes;
      List.map (fun p -> test_prime_fermat__when_prime__then_false p) params_primes ]

(* test_primes_miller_Test_list *)

let test_prime_miller__when_not_prime__then_false param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ param in
  label >:: fun _ ->
    (* when *)
    let result = test_prime_miller param in
    (* then *)
    assert_that result Is.false_

let test_prime_miller__when_prime__then_false param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ param in
  label >:: fun _ ->
    (* when *)
    let result = test_prime_miller param in
    (* then *)
    assert_that result Is.true_

let test_primes_miller_Test_list =
  test_list
  @@ List.concat
    [ List.map (fun p -> test_prime_miller__when_not_prime__then_false p) params_not_primes;
      List.map (fun p -> test_prime_miller__when_prime__then_false p) params_primes ]

(* primes_testing_Test *)

let primes_testing_Test =
  "Tests: Algorithms for testing prime numbers"
  >::: [test_primes_fermat_Test_list; test_primes_miller_Test_list]

let _ = run_test_tt_main primes_testing_Test
