(* Algorithms for basic mathematical computations *)

let gcd number1 number2 =
  let rec gcd' n1 n2 = if n1 < n2 then gcd' n2 n1 else if n2 = 0 then n1 else gcd' n2 (n1 mod n2) in
  gcd' (abs number1) (abs number2)

let ( **/ ) = gcd

let lcm number1 number2 =
  let lcm' n1 n2 = max n1 n2 / (n1 **/ n2) * min n1 n2 in
  lcm' (abs number1) (abs number2)

let ( **^ ) = lcm

let multiply factor1 factor2 =
  let rec mult' fc1 fc2 res step =
    if fc2 > 0
    then
      let fc1'', res'' = step fc1 res in
      if fc2 mod 2 = 1 then mult' fc1'' (fc2 / 2) res'' step else mult' fc1'' (fc2 / 2) res step
    else res
  in
  let step fc1' res' = (fc1' + fc1', fc1' + res') in
  if factor1 < 0 && factor2 < 0
  then mult' (-factor1) (-factor2) 0 step
  else if factor1 < 0
  then -mult' (-factor1) factor2 0 step
  else if factor2 < 0
  then -mult' factor1 (-factor2) 0 step
  else mult' factor1 factor2 0 step

let multiply_mod factor1 factor2 modulo =
  let rec mult' fc1 fc2 res step =
    if fc2 > 0
    then
      let fc1'', res'' = step fc1 res in
      if fc2 mod 2 = 1 then mult' fc1'' (fc2 / 2) res'' step else mult' fc1'' (fc2 / 2) res step
    else res
  in
  if modulo <= 0
  then failwith "Non-positive modulo"
  else
    let step_mod fc1' res' = ((fc1' + fc1') mod modulo, (fc1' + res') mod modulo) in
    if factor1 < 0 && factor2 < 0
    then mult' (-factor1) (-factor2) 0 step_mod
    else if factor1 < 0
    then modulo - mult' (-factor1) factor2 0 step_mod
    else if factor2 < 0
    then modulo - mult' factor1 (-factor2) 0 step_mod
    else mult' factor1 factor2 0 step_mod

let power base expon =
  let rec power' base' expon' res' =
    if expon' > 0
    then
      if expon' mod 2 = 1
      then power' (multiply base' base') (expon' / 2) (multiply base' res')
      else power' (multiply base' base') (expon' / 2) res'
    else res'
  in
  if expon < 0
  then failwith "Negative exponent"
  else if base = 0 && expon = 0
  then failwith "Not a number"
  else power' base expon 1

let power_mod base expon modulo =
  let rec power' base' expon' res' =
    if expon' > 0
    then
      if expon' mod 2 = 1
      then power' (multiply_mod base' base' modulo) (expon' / 2) (multiply_mod base' res' modulo)
      else power' (multiply_mod base' base' modulo) (expon' / 2) res'
    else res'
  in
  if modulo <= 0
  then failwith "Non-positive modulo"
  else if expon < 0
  then failwith "Negative exponent"
  else if base = 0 && expon = 0
  then failwith "Not a number"
  else power' base expon 1
