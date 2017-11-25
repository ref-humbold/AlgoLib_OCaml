(* ALGORYTMY MATEMATYCZNE *)

(* Największy wspólny dzielnik dwóch liczb.
   @return największy wspólny dzielnik *)
let rec gcdiv number1 number2 =
  if number1 < number2
  then gcdiv number2 number1
  else if number2 == 0
  then number1
  else gcdiv number2 @@ number1 mod number2;;

(* Najmniejsza wspólna wielokrotność dwóch liczb.
   @return najmniejsza wspólna wielokrotność *)
let lcm number1 number2 =
  (max number1 number2) / (gcdiv number1 number2) * (min number1 number2);;

(* Szybkie mnożenie binarne modulowane.
   @return wynik mnożenia *)
let mult_mod factor1 factor2 modulo =
  let rec mult_ factor1_ factor2_ modulo_ result =
    if factor2_ > 0
    then let mul_aux mdl res = mult_ ((factor1_ + factor1_) mod modulo_) (factor2_ / 2) mdl res in
      if factor2_ mod 2 == 1
      then mul_aux modulo_ ((factor1_ + result) mod modulo_)
      else mul_aux modulo_ result
    else if factor2_ == 0
    then result
    else modulo - (mult_ factor1_ (-factor2_) modulo_ result) in
  mult_ factor1 factor2 modulo 0;;

(* Szybkie potęgowanie binarne modulowane.
   @return wynik potęgowania *)
let power_mod base expon modulo =
  let rec power_ base_ expon_ modulo_ result =
    if expon_ > 0
    then
      let pow_aux res = power_ (mult_mod base_ base_ modulo_) (expon_ / 2) modulo_ res in
      if expon_ mod 2 == 1
      then pow_aux @@ mult_mod result base_ modulo_
      else pow_aux result
    else if expon_ == 0
    then if base_ == 0 then failwith "nan" else result
    else failwith "Negative exponent" in
  power_ base expon modulo 1;;
