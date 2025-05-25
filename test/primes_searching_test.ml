(* Tests: Algorithms for searching for prime numbers. *)

open OUnit2
open OAssert
open Algolib.Maths.Primes_searching

let[@ocamlformat "break-collection-expressions = wrap"] primes =
  [ 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71; 73; 79; 83; 89; 97;
    101; 103; 107; 109; 113; 127; 131; 137; 139; 149; 151; 157; 163; 167; 173; 179; 181; 191; 193;
    197; 199; 211; 223; 227; 229; 233; 239; 241; 251; 257; 263; 269; 271; 277; 281; 283; 293; 307;
    311; 313; 317; 331; 337; 347; 349; 353; 359; 367; 373; 379; 383; 389; 397; 401; 409; 419; 421;
    431; 433; 439; 443; 449; 457; 461; 463; 467; 479; 487; 491; 499; 503; 509; 521; 523; 541; 547;
    557; 563; 569; 571; 577; 587; 593; 599; 601; 607; 613; 617; 619; 631; 641; 643; 647; 653; 659;
    661; 673; 677; 683; 691; 701; 709; 719; 727; 733; 739; 743; 751; 757; 761; 769; 773; 787; 797;
    809; 811; 821; 823; 827; 829; 839; 853; 857; 859; 863; 877; 881; 883; 887; 907; 911; 919; 929;
    937; 941; 947; 953; 967; 971; 977; 983; 991; 997 ]

let params_max = [2; 3; 4; 67; 100; 155; 400; 499; 701; 911]

let params_min_max =
  let product lst1 lst2 = List.concat_map (fun e1 -> List.map (fun e2 -> (e1, e2)) lst2) lst1 in
  product [2; 3; 8; 25; 54; 71; 101; 243] [54; 150; 243; 481; 625; 827; 1000]

module IsList = Is.List.Of (Type.Int)

(* find_primes_Test_list *)

let find_primes__when_maximal_number__then_max_exclusive param =
  let label = Printf.sprintf "%s [param = %d]" __FUNCTION__ param in
  label >:: fun _ ->
    (* when *)
    let result = find_primes 0 param in
    (* then *)
    let expected = List.filter (fun p -> p < param) primes in
    assert_that result @@ IsList.equal_to expected

let find_primes__when_range__then_min_inclusive_and_max_exclusive (minimum, maximum) =
  let label = Printf.sprintf "%s [param = (%d, %d)]" __FUNCTION__ minimum maximum in
  label >:: fun _ ->
    (* when *)
    let result = find_primes minimum maximum in
    (* then *)
    let expected = List.filter (fun p -> p >= minimum && p < maximum) primes in
    assert_that result @@ IsList.equal_to expected

let find_primes_Test_list =
  test_list
  @@ List.concat
    [ List.map (fun p -> find_primes__when_maximal_number__then_max_exclusive p) params_max;
      List.map
        (fun p -> find_primes__when_range__then_min_inclusive_and_max_exclusive p)
        params_min_max ]

(* primes_testing_Test *)

let primes_searching_Test =
  "Tests: Algorithms for searching for prime numbers" >::: [find_primes_Test_list]

let _ = run_test_tt_main primes_searching_Test
