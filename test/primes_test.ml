(* Tests: Algorithms for prime numbers *)
open OUnit2
open Algolib.Primes

(* test_fermat_Test_list *)

let test_fermat_When_zero_Then_false =
  "test_fermat When zero Then false"
  >:: fun _ ->
    let result = test_fermat 0 in
    assert_bool "Expected false value" @@ not result

let test_fermat_When_one_Then_false =
  "test_fermat When one Then false"
  >:: fun _ ->
    let result = test_fermat 1 in
    assert_bool "Expected false value" @@ not result

let test_fermat_When_two_Then_true =
  "test_fermat When two Then true"
  >:: fun _ ->
    let result = test_fermat 2 in
    assert_bool "Expected true value" result

let test_fermat_When_prime_Then_true =
  "test_fermat When prime Then true"
  >:: fun _ ->
    let result = test_fermat 1013 in
    assert_bool "Expected true value" result

let test_fermat_When_composite_Then_false =
  "test_fermat When composite Then false"
  >:: fun _ ->
    let result = test_fermat 1001 in
    assert_bool "Expected false value" @@ not result

let test_fermat_When_carmichael_number_Then_false =
  "test_fermat When carmichael number Then false"
  >:: fun _ ->
    (* 1105 = 5 * 13 * 17 is a Carmichael number *)
    let result = test_fermat 1105 in
    assert_bool "Expected false value" @@ not result

let test_fermat_Test_list =
  test_list
    [ test_fermat_When_zero_Then_false; test_fermat_When_one_Then_false;
      test_fermat_When_two_Then_true; test_fermat_When_prime_Then_true;
      test_fermat_When_composite_Then_false; test_fermat_When_carmichael_number_Then_false ]

(* test_miller_Test_list *)

let test_miller_When_zero_Then_false =
  "test_miller When zero Then false"
  >:: fun _ ->
    let result = test_miller 0 in
    assert_bool "Expected false value" @@ not result

let test_miller_When_one_Then_false =
  "test_miller When one Then false"
  >:: fun _ ->
    let result = test_miller 1 in
    assert_bool "Expected false value" @@ not result

let test_miller_When_two_Then_true =
  "test_miller When two Then true"
  >:: fun _ ->
    let result = test_miller 2 in
    assert_bool "Expected true value" result

let test_miller_When_prime_Then_true =
  "test_miller_When_prime_Then_true"
  >:: fun _ ->
    let result = test_miller 1013 in
    assert_bool "Expected true value" result

let test_miller_When_composite_Then_false =
  "test_miller When composite Then false"
  >:: fun _ ->
    let result = test_miller 1001 in
    assert_bool "Expected false value" @@ not result

let test_miller_When_carmichael_number_Then_false =
  "test_miller When carmichael number Then false"
  >:: fun _ ->
    (* 1105 = 5 * 13 * 17 is a Carmichael number *)
    let result = test_miller 1105 in
    assert_bool "Expected false value" @@ not result

let test_miller_Test_list =
  test_list
    [ test_miller_When_zero_Then_false; test_miller_When_one_Then_false;
      test_miller_When_two_Then_true; test_miller_When_prime_Then_true;
      test_miller_When_composite_Then_false; test_miller_When_carmichael_number_Then_false ]

(* primes_Test *)

let primes_Test =
  "Tests: Algorithms for prime numbers" >::: [test_fermat_Test_list; test_miller_Test_list]

let _ = run_test_tt_main primes_Test
