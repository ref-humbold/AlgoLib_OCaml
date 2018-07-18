(* ALGORITHMS FOR PRIME NUMBERS *)

let rec rands' x n =
  let () = Random.self_init () in
  if n = 0
  then []
  else (1 + Random.int (x - 1))::(rands' x (n - 1))

let test_fermat number =
  if number = 2 || number = 3
  then true
  else if number < 2 || number mod 2 = 0 || number mod 3 = 0
  then false
  else not @@ List.exists (fun rdv -> Maths.gcdiv rdv number > 1 || Maths.power_mod rdv (number - 1) number <> 1) @@ rands' number 12

let distribute' number =
  let rec distribute'_ e p =
    if number mod p = 0
    then (e - 1, number / (1 lsl (e - 1)))
    else distribute'_ (e + 1) (p lsl 1)
  in distribute'_ 1 2

let test_miller number =
  if number = 2 || number = 3
  then true
  else if number < 2 || number mod 2 = 0 || number mod 3 = 0
  then false
  else
    let rec range n lst =
      if n = 0
      then 0::lst
      else range (n - 1) @@ n::lst in
    let (e, m) = distribute' (number - 1) in
    not @@ List.exists (fun rdv -> Maths.power_mod rdv m number <> 1 && List.for_all (fun i -> (Maths.power_mod rdv ((1 lsl i) * m) number <> number - 1)) (range (e - 1) [])) @@ rands' number 12
