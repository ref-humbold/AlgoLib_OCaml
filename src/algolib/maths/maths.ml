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
  let rec multiply' factor1' factor2' res step =
    if factor2' > 0
    then
      let factor1'', res' = step factor1' res in
      if factor2' mod 2 = 1
      then multiply' factor1'' (factor2' / 2) res' step
      else multiply' factor1'' (factor2' / 2) res step
    else res
  in
  let step fc1' res' = (fc1' + fc1', fc1' + res') in
  if factor1 < 0 && factor2 < 0
  then multiply' (-factor1) (-factor2) 0 step
  else if factor1 < 0
  then -multiply' (-factor1) factor2 0 step
  else if factor2 < 0
  then -multiply' factor1 (-factor2) 0 step
  else multiply' factor1 factor2 0 step

let multiply_mod factor1 factor2 modulo =
  let rec multiply' factor1' factor2' result step =
    if factor2' > 0
    then
      let factor1'', result' = step factor1' result in
      if factor2' mod 2 = 1
      then multiply' factor1'' (factor2' / 2) result' step
      else multiply' factor1'' (factor2' / 2) result step
    else result
  in
  if modulo <= 0
  then failwith "Non-positive modulo"
  else
    let step_mod factor1' result =
      ((factor1' + factor1') mod modulo, (factor1' + result) mod modulo)
    in
    if factor1 < 0 && factor2 < 0
    then multiply' (-factor1) (-factor2) 0 step_mod
    else if factor1 < 0
    then modulo - multiply' (-factor1) factor2 0 step_mod
    else if factor2 < 0
    then modulo - multiply' factor1 (-factor2) 0 step_mod
    else multiply' factor1 factor2 0 step_mod

let power base exponent =
  let rec power' base' exponent' result =
    if exponent' > 0
    then
      if exponent' mod 2 = 1
      then power' (multiply base' base') (exponent' / 2) (multiply base' result)
      else power' (multiply base' base') (exponent' / 2) result
    else result
  in
  if exponent < 0
  then failwith "Negative exponent"
  else if base = 0 && exponent = 0
  then failwith "Not a number"
  else power' base exponent 1

let power_mod base exponent modulo =
  let rec power' base' exponent' result =
    if exponent' > 0
    then
      if exponent' mod 2 = 1
      then
        power' (multiply_mod base' base' modulo) (exponent' / 2) (multiply_mod base' result modulo)
      else power' (multiply_mod base' base' modulo) (exponent' / 2) result
    else result
  in
  if modulo <= 0
  then failwith "Non-positive modulo"
  else if exponent < 0
  then failwith "Negative exponent"
  else if base = 0 && exponent = 0
  then failwith "Not a number"
  else power' base exponent 1
