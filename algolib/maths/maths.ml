(* MATHS ALGORITHMS *)

let rec gcdiv number1 number2 =
  if number1 < number2
  then gcdiv number2 number1
  else if number2 == 0
  then number1
  else gcdiv number2 @@ number1 mod number2;;

let lcm number1 number2 =
  (max number1 number2) / (gcdiv number1 number2) * (min number1 number2);;

let mult_mod factor1 factor2 modulo =
  let rec mult_ fc1 fc2 res =
    if fc2 > 0
    then let (nfc1, nres) =
           if modulo = 0
           then (fc1 + fc1, fc1 + res)
           else ((fc1 + fc1) mod modulo, (fc1 + res) mod modulo) in
      if fc2 mod 2 == 1
      then mult_ nfc1 (fc2 / 2) nres
      else mult_ nfc1 (fc2 / 2) res
    else res in
  if modulo < 0
  then failwith "Negative modulo."
  else if factor1 < 0 && factor2 < 0
  then mult_ (-factor1) (-factor2) 0
  else if factor1 < 0
  then modulo - (mult_ (-factor1) factor2 0)
  else if factor2 < 0
  then modulo - (mult_ factor1 (-factor2) 0)
  else mult_ factor1 factor2 0;;

let power_mod base expon modulo =
  let rec power_ base_ expon_ res =
    if expon_ > 0
    then if expon_ mod 2 == 1
      then power_ (mult_mod base_ base_ modulo) (expon_ / 2) (mult_mod base_ res modulo)
      else power_ (mult_mod base_ base_ modulo) (expon_ / 2) res
    else res in
  if modulo < 0
  then failwith "Negative modulo."
  else if expon < 0
  then failwith "Negative exponent."
  else if base = 0 && expon = 0
  then failwith "Not a number."
  else power_ base expon 1;;
