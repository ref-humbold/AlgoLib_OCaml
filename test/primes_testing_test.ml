(* Tests: Algorithms for testing prime numbers *)
open OUnit2
open Algolib.Primes_testing

(* test_primes_fermat_Test_list *)

let test_primes_fermat__when_zero__then_false =
  "test_primes_fermat When zero Then false" >:: fun _ ->
    (* when *)
    let result = test_primes_fermat 0 in
    (* then *)
    assert_bool "Expected false value" @@ not result

let test_primes_fermat__when_one__then_false =
  "test_primes_fermat When one Then false" >:: fun _ ->
    (* when *)
    let result = test_primes_fermat 1 in
    (* then *)
    assert_bool "Expected false value" @@ not result

let test_primes_fermat__when_two__then_true =
  "test_primes_fermat When two Then true" >:: fun _ ->
    (* when *)
    let result = test_primes_fermat 2 in
    (* then *)
    assert_bool "Expected true value" result

let test_primes_fermat__when_prime__then_true =
  "test_primes_fermat When prime Then true" >:: fun _ ->
    (* when *)
    let result = test_primes_fermat 1013 in
    (* then *)
    assert_bool "Expected true value" result

let test_primes_fermat__when_composite1__then_false =
  "test_primes_fermat When composite1 Then false" >:: fun _ ->
    (* when *)
    let result = test_primes_fermat 1001 in
    (* then *)
    assert_bool "Expected false value" @@ not result

let test_primes_fermat__when_carmichael_number__then_false =
  "test_primes_fermat When carmichael number Then false" >:: fun _ ->
    (* when *)
    (* 41041 = 7 * 11 * 13 * 41 is a Carmichael number *)
    let result = test_primes_fermat 41041 in
    (* then *)
    assert_bool "Expected false value" @@ not result

let test_primes_fermat_Test_list =
  test_list
    [ test_primes_fermat__when_zero__then_false;
      test_primes_fermat__when_one__then_false;
      test_primes_fermat__when_two__then_true;
      test_primes_fermat__when_prime__then_true;
      test_primes_fermat__when_composite1__then_false;
      test_primes_fermat__when_carmichael_number__then_false ]

(* test_primes_miller_Test_list *)

let test_primes_miller__when_zero__then_false =
  "test_primes_miller When zero Then false" >:: fun _ ->
    (* when *)
    let result = test_primes_miller 0 in
    (* then *)
    assert_bool "Expected false value" @@ not result

let test_primes_miller__when_one__then_false =
  "test_primes_miller When one Then false" >:: fun _ ->
    (* when *)
    let result = test_primes_miller 1 in
    (* then *)
    assert_bool "Expected false value" @@ not result

let test_primes_miller__when_two__then_true =
  "test_primes_miller When two Then true" >:: fun _ ->
    (* when *)
    let result = test_primes_miller 2 in
    (* then *)
    assert_bool "Expected true value" result

let test_primes_miller__when_prime__then_true =
  "test_primes_miller__when_prime__then_true" >:: fun _ ->
    (* when *)
    let result = test_primes_miller 1013 in
    (* then *)
    assert_bool "Expected true value" result

let test_primes_miller__when_composite1__then_false =
  "test_primes_miller When composite1 Then false" >:: fun _ ->
    (* when *)
    let result = test_primes_miller 1001 in
    (* then *)
    assert_bool "Expected false value" @@ not result

let test_primes_miller__when_composite2__then_false =
  "test_primes_miller When composite2 Then false" >:: fun _ ->
    (* when *)
    let result = test_primes_miller 41041 in
    (* then *)
    assert_bool "Expected false value" @@ not result

let test_primes_miller_Test_list =
  test_list
    [ test_primes_miller__when_zero__then_false;
      test_primes_miller__when_one__then_false;
      test_primes_miller__when_two__then_true;
      test_primes_miller__when_prime__then_true;
      test_primes_miller__when_composite1__then_false;
      test_primes_miller__when_composite2__then_false ]

(* primes_Test *)

let primes_Test =
  "Tests: Algorithms for prime numbers"
  >::: [test_primes_fermat_Test_list; test_primes_miller_Test_list]

let _ = run_test_tt_main primes_Test
