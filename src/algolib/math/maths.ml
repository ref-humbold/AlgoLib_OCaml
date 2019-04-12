(* MATHS ALGORITHMS *)

let rec gcdiv number1 number2 =
  if number1 < number2
  then gcdiv number2 number1
  else if number2 = 0
  then number1
  else gcdiv number2 @@ number1 mod number2

let lcm number1 number2 =
  (max number1 number2) / (gcdiv number1 number2) * (min number1 number2)

let mult_mod factor1 factor2 modulo =
  let rec mult' fc1 fc2 res =
    if fc2 > 0
    then let (nfc1, nres) =
           if modulo = 0
           then (fc1 + fc1, fc1 + res)
           else ((fc1 + fc1) mod modulo, (fc1 + res) mod modulo) in
      if fc2 mod 2 = 1
      then mult' nfc1 (fc2 / 2) nres
      else mult' nfc1 (fc2 / 2) res
    else res in
  if modulo < 0
  then failwith "Negative modulo."
  else if factor1 < 0 && factor2 < 0
  then mult' (-factor1) (-factor2) 0
  else if factor1 < 0
  then modulo - (mult' (-factor1) factor2 0)
  else if factor2 < 0
  then modulo - (mult' factor1 (-factor2) 0)
  else mult' factor1 factor2 0

let power_mod base expon modulo =
  let rec power' base' expon' res =
    if expon' > 0
    then if expon' mod 2 = 1
      then power' (mult_mod base' base' modulo) (expon' / 2) (mult_mod base' res modulo)
      else power' (mult_mod base' base' modulo) (expon' / 2) res
    else res in
  if modulo < 0
  then failwith "Negative modulo."
  else if expon < 0
  then failwith "Negative exponent."
  else if base = 0 && expon = 0
  then failwith "Not a number."
  else power' base expon 1