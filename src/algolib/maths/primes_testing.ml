(* Algorithms for testing prime numbers *)
open Maths

let rands_ maximum num =
  Random.self_init () ;
  let rec rands_' m' n' = if n' = 0 then [] else (1 + Random.int m') :: rands_' m' (n' - 1) in
  rands_' (maximum - 1) num

let test_primes_fermat number =
  let n = abs number in
  if n = 2 || n = 3
  then true
  else if n < 2 || n mod 2 = 0 || n mod 3 = 0
  then false
  else List.for_all (fun r -> r **/ n == 1 && power_mod r (n - 1) n == 1) @@ rands_ n 12

let test_primes_miller number =
  let n = abs number in
  if n = 2 || n = 3
  then true
  else if n < 2 || n mod 2 = 0 || n mod 3 = 0
  then false
  else
    let rec remove_twos n' = if n' mod 2 == 0 then remove_twos (n' / 2) else n' in
    let rec mult_twos acc d' = if d' <= n / 2 then mult_twos (d' :: acc) (d' * 2) else acc in
    let d = remove_twos (n - 1) in
    List.for_all (fun r ->
                   power_mod r d n == 1 || List.exists (fun d' -> power_mod r d' n == n - 1) (mult_twos [] d) )
    @@ rands_ n 12
