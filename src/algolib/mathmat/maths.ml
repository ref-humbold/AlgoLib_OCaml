(* Basic mathematics algorithms. *)

let gcdiv number1 number2 =
  let rec gcdiv' n1 n2 =
    if n1 < n2 then gcdiv' n2 n1 else if n2 = 0 then n1 else gcdiv' n2 (n1 mod n2)
  in
  gcdiv' (abs number1) (abs number2)

let ( **/ ) = gcdiv

let lcmul number1 number2 =
  let lcmul' n1 n2 = max n1 n2 / (n1 **/ n2) * min n1 n2 in
  lcmul' (abs number1) (abs number2)

let ( **^ ) = lcmul

let mult_mod ?(modulo = 0) factor1 factor2 =
  let rec mult' fc1 fc2 res step =
    if fc2 > 0
    then
      let fc1'', res'' = step fc1 res in
      if fc2 mod 2 = 1 then mult' fc1'' (fc2 / 2) res'' step else mult' fc1'' (fc2 / 2) res step
    else res
  in
  if modulo < 0
  then failwith "Negative modulo"
  else
    let step_mod =
      if modulo = 0
      then fun fc1' res' -> (fc1' + fc1', fc1' + res')
      else fun fc1' res' -> ((fc1' + fc1') mod modulo, (fc1' + res') mod modulo)
    in
    if factor1 < 0 && factor2 < 0
    then mult' (-factor1) (-factor2) 0 step_mod
    else if factor1 < 0
    then modulo - mult' (-factor1) factor2 0 step_mod
    else if factor2 < 0
    then modulo - mult' factor1 (-factor2) 0 step_mod
    else mult' factor1 factor2 0 step_mod

let power_mod ?(modulo = 0) base expon =
  let rec power' base' expon' res' =
    if expon' > 0
    then
      if expon' mod 2 = 1
      then power' (mult_mod ~modulo base' base') (expon' / 2) (mult_mod ~modulo base' res')
      else power' (mult_mod ~modulo base' base') (expon' / 2) res'
    else res'
  in
  if modulo < 0
  then failwith "Negative modulo"
  else if expon < 0
  then failwith "Negative exponent"
  else if base = 0 && expon = 0
  then failwith "Not a number"
  else power' base expon 1
