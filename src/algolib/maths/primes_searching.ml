(* Algorithms for searching for prime numbers. *)

let get_base_primes_ base_maximum =
  let mark_multiples array value =
    let rec mark_multiples' i =
      if i <= base_maximum
      then (
        array.((i - 3) / 2) <- false ;
        mark_multiples' (i + (2 * value)) )
      else ()
    in
    mark_multiples' (value * value)
  in
  let is_prime = Array.init ((base_maximum - 1) / 2) (fun _ -> true) in
  let loop_end = int_of_float (sqrt (float_of_int base_maximum) /. 2.0) - 1 in
  for i = 0 to loop_end do
    if is_prime.(i)
    then
      let prime_value = (2 * i) + 3 in
      mark_multiples is_prime prime_value
  done ;
  List.filter_map (fun (i, e) -> if e then Some ((2 * i) + 3) else None)
  @@ List.mapi (fun i e -> (i, e))
  @@ Array.to_list is_prime

let get_segment_primes_ segment_start segment_end base_primes =
  let segment_begin = segment_start + 1 - (segment_start mod 2) in
  let rec mark_multiples array prime i =
    if i < segment_end
    then (
      array.((i - segment_begin) / 2) <- false ;
      mark_multiples array prime (i + (2 * prime)) )
    else ()
  in
  let rec create_is_primes i acc =
    if i < segment_end
    then create_is_primes (i + 2) ((i > 2) :: acc)
    else Array.of_list @@ List.rev acc
  in
  let is_prime = create_is_primes segment_begin [] in
  List.iter
    (fun p ->
       let prime_multiple = (segment_begin + p - 1) / p * p in
       let multiple_start =
         if prime_multiple mod 2 == 0 then prime_multiple + p else prime_multiple
       in
       mark_multiples is_prime p multiple_start )
    base_primes ;
  List.filter_map (fun (i, e) -> if e then Some (segment_begin + (2 * i)) else None)
  @@ List.mapi (fun i e -> (i, e))
  @@ Array.to_list is_prime

let find_primes minimum maximum =
  if maximum <= minimum || maximum <= 2 || minimum < 0
  then []
  else
    let segment_size = int_of_float @@ sqrt @@ float_of_int maximum in
    let base_primes = get_base_primes_ segment_size in
    let primes =
      if minimum < segment_size
      then
        let filtered_base = List.filter (fun p -> p >= minimum) base_primes in
        if 2 >= minimum then 2 :: filtered_base else filtered_base
      else []
    in
    let rec append_segments i acc =
      if i < maximum
      then
        let segment_primes = get_segment_primes_ i (min (i + segment_size) maximum) base_primes in
        append_segments (i + segment_size) (List.rev_append segment_primes acc)
      else List.rev acc
    in
    append_segments (max minimum segment_size) (List.rev primes)
