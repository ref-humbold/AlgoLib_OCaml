(* Algorithms for prime numbers. *)
open Maths

let rands_ maximum num =
  let rec rands_' m' n' = if n' = 0 then [] else (1 + Random.int m') :: rands_' m' (n' - 1) in
  Random.self_init () ;
  rands_' (maximum - 1) num

let test_fermat number =
  let n = abs number in
  if n = 2 || n = 3
  then true
  else if n < 2 || n mod 2 = 0 || n mod 3 = 0
  then false
  else List.for_all (fun r -> r **/ n == 1 && power_mod ~modulo:n r (n - 1) == 1) @@ rands_ n 12

let test_miller number =
  let n = abs number in
  if n = 2 || n = 3
  then true
  else if n < 2 || n mod 2 = 0 || n mod 3 = 0
  then false
  else
    let rec remove_twos n' = if n' mod 2 == 0 then remove_twos (n' / 2) else n' in
    let rec mult_twos acc d' = if d' <= n / 2 then mult_twos (d' :: acc) (d' lsl 1) else acc in
    let d = remove_twos (n - 1) in
    List.for_all (fun r ->
                   power_mod ~modulo:n r d == 1
                   || List.exists (fun d' -> power_mod ~modulo:n r d' == n - 1) (mult_twos [] d))
    @@ rands_ n 12
