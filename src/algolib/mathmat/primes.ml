(* Algorithms for prime numbers. *)
open Maths

let rands_ maxim num =
  let rec rands_' m' n' = if n' = 0 then [] else (1 + Random.int m') :: rands_' m' (n' - 1) in
  Random.self_init () ;
  rands_' (maxim - 1) num

let test_fermat number =
  let n = abs number in
  if n = 2 || n = 3
  then true
  else if n < 2 || n mod 2 = 0 || n mod 3 = 0
  then false
  else
    List.for_all (fun rdv -> rdv **/ n == 1 && power_mod ~modulo:n rdv (n - 1) == 1) @@ rands_ n 12

let distribute_ n =
  let rec distribute_' e p =
    if n mod p = 0 then (e - 1, n / (1 lsl (e - 1))) else distribute_' (e + 1) (p lsl 1)
  in
  distribute_' 1 2

let test_miller number =
  let n = abs number in
  if n = 2 || n = 3
  then true
  else if n < 2 || n mod 2 = 0 || n mod 3 = 0
  then false
  else
    let rec range lst n = if n <= 1 then 0 :: lst else range ((n - 1) :: lst) (n - 1) in
    let s, d = distribute_ (n - 1) in
    List.for_all (fun rdv ->
                   power_mod ~modulo:n rdv d == 1
                   || List.exists (fun s' -> power_mod ~modulo:n rdv ((1 lsl s') * d) == n - 1) (range [] s))
    @@ rands_ n 12
