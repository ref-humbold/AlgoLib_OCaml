(* Algorithms for prime numbers. *)

let rands_ maxim num =
  let rec rands_' m' n' = if n' = 0 then [] else (1 + Random.int m') :: (rands_' m' @@ n' - 1) in
  begin
    Random.self_init ();
    rands_' (maxim - 1) num
  end

let test_fermat number =
  if number = 2 || number = 3
  then true
  else if number < 2 || number mod 2 = 0 || number mod 3 = 0
  then false
  else List.for_all (fun rdv -> Maths.gcdiv rdv number == 1
                                && Maths.power_mod rdv (number - 1) number == 1)
    @@ rands_ number 12

let distribute_ n =
  let rec distribute_' e p =
    if n mod p = 0
    then (e - 1, n / (1 lsl (e - 1)))
    else distribute_' (e + 1) (p lsl 1)
  in distribute_' 1 2

let test_miller number =
  if number = 2 || number = 3
  then true
  else if number < 2 || number mod 2 = 0 || number mod 3 = 0
  then false
  else
    let rec range lst n =
      if n <= 1
      then 0 :: lst
      else range ((n - 1) :: lst) @@ n - 1 in
    let s, d = distribute_ (number - 1) in
    List.for_all (fun rdv ->
        Maths.power_mod rdv d number == 1
        || List.exists (fun s' -> (Maths.power_mod rdv ((1 lsl s') * d) number == number - 1))
          (range [] s))
    @@ rands_ number 12
