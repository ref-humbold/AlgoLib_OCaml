(* Mathematics algorithms. *)

let gcdiv number1 number2 =
  let rec gcdiv' n1 n2 =
    if n1 < n2 then gcdiv' n2 n1 else if n2 = 0 then n1 else gcdiv' n2 (n1 mod n2)
  in
  gcdiv' (abs number1) (abs number2)

let lcm number1 number2 =
  let lcm' n1 n2 = max n1 n2 / gcdiv n1 n2 * min n1 n2 in
  lcm' (abs number1) (abs number2)

let mult_mod factor1 factor2 modulo =
  let rec mult' factor1' factor2' res' step' =
    if factor2' > 0
    then
      let fc1'', res'' = step' factor1' res' in
      if factor2' mod 2 = 1
      then mult' fc1'' (factor2' / 2) res'' step'
      else mult' fc1'' (factor2' / 2) res' step'
    else res'
  in
  if modulo < 0
  then failwith "Negative modulo"
  else
    let step =
      if modulo = 0
      then fun fc1' res' -> (fc1' + fc1', fc1' + res')
      else fun fc1' res' -> ((fc1' + fc1') mod modulo, (fc1' + res') mod modulo)
    in
    if factor1 < 0 && factor2 < 0
    then mult' (-factor1) (-factor2) 0 step
    else if factor1 < 0
    then modulo - mult' (-factor1) factor2 0 step
    else if factor2 < 0
    then modulo - mult' factor1 (-factor2) 0 step
    else mult' factor1 factor2 0 step

let power_mod base expon modulo =
  let rec power' base' expon' res' =
    if expon' > 0
    then
      if expon' mod 2 = 1
      then power' (mult_mod base' base' modulo) (expon' / 2) (mult_mod base' res' modulo)
      else power' (mult_mod base' base' modulo) (expon' / 2) res'
    else res'
  in
  if modulo <= 0
  then failwith "Non-positive modulo"
  else if expon < 0
  then failwith "Negative exponent"
  else if base = 0 && expon = 0
  then failwith "Not a number"
  else power' base expon 1
