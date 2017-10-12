(* PODCIĄG SPÓJNY O MAKSYMALNEJ SUMIE LICZONY DYNAMICZNIE *)

(* Wyznaczanie spójnego podciągu o maksymalnej sumie.
   @return elementy podciągu *)
let findMaximumSubarray seque =
  let subarray sq actualSubarray maxSubarray =
    match sq with
    | e::es ->
      if sum actualSubarray < 0 then subarray [] maxSubarray sq
      else let newSubarray = (e::actualSubarray) in
        if (sum newSubarray) > (sum maxSubarray)
        then subarray newSubarray newSubarray es
        else subarray newSubarray maxSubarray es
    | [] -> reverse maxSubarray in
  subarray seque [] [];;
