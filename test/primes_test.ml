(* Tests: Algorithms for prime numbers. *)
open OUnit2
open Algolib.Primes

(* test_fermat *)

let test_fermat_When_zero_Then_false =
  "Apply Fermat test to number 0"
  >:: fun _ ->
    let result = test_fermat 0 in
    assert_bool "Expected false value" @@ not result

let test_fermat_When_one_Then_false =
  "Apply Fermat test to number 1"
  >:: fun _ ->
    let result = test_fermat 1 in
    assert_bool "Expected false value" @@ not result

let test_fermat_When_two_Then_true =
  "Apply Fermat test to number 2"
  >:: fun _ ->
    let result = test_fermat 2 in
    assert_bool "Expected true value" result

let test_fermat_When_prime_Then_true =
  "Apply Fermat test to a prime number"
  >:: fun _ ->
    let result = test_fermat 1013 in
    assert_bool "Expected true value" result

let test_fermat_When_composite_Then_false =
  "Apply Fermat test to a composite number"
  >:: fun _ ->
    let result = test_fermat 1001 in
    assert_bool "Expected false value" @@ not result

let test_fermat_When_carmichael_number_Then_false =
  "Apply Fermat test to a Carmichael number"
  >:: fun _ ->
    (* 1105 = 5 * 13 * 17 is a Carmichael number *)
    let result = test_fermat 1105 in
    assert_bool "Expected false value" @@ not result

let test_fermat_Test =
  test_list
    [ test_fermat_When_zero_Then_false; test_fermat_When_one_Then_false;
      test_fermat_When_two_Then_true; test_fermat_When_prime_Then_true;
      test_fermat_When_composite_Then_false; test_fermat_When_carmichael_number_Then_false ]

(* test_miller *)

let test_miller_When_zero_Then_false =
  "Apply Fermat test to number 0"
  >:: fun _ ->
    let result = test_miller 0 in
    assert_bool "Expected false value" @@ not result

let test_miller_When_one_Then_false =
  "Apply Fermat test to number 1"
  >:: fun _ ->
    let result = test_miller 1 in
    assert_bool "Expected false value" @@ not result

let test_miller_When_two_Then_true =
  "Apply Fermat test to number 2"
  >:: fun _ ->
    let result = test_miller 2 in
    assert_bool "Expected true value" result

let test_miller_When_prime_Then_true =
  "Apply Fermat test to a prime number"
  >:: fun _ ->
    let result = test_miller 1013 in
    assert_bool "Expected true value" result

let test_miller_When_composite_Then_false =
  "Apply Fermat test to a composite number"
  >:: fun _ ->
    let result = test_miller 1001 in
    assert_bool "Expected false value" @@ not result

let test_miller_When_carmichael_number_Then_false =
  "Apply Fermat test to a Carmichael number"
  >:: fun _ ->
    (* 1105 = 5 * 13 * 17 is a Carmichael number *)
    let result = test_miller 1105 in
    assert_bool "Expected false value" @@ not result

let test_miller_Test =
  test_list
    [ test_miller_When_zero_Then_false; test_miller_When_one_Then_false;
      test_miller_When_two_Then_true; test_miller_When_prime_Then_true;
      test_miller_When_composite_Then_false; test_miller_When_carmichael_number_Then_false ]

(* primes test *)

let primes_Test = "Tests: Algorithms for prime numbers" >::: [test_fermat_Test; test_miller_Test]

let _ = run_test_tt_main primes_Test
