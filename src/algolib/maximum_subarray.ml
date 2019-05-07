(* Dynamic algorithm for maximum subarray. *)
let find_maximum_subarray sequence =
  let push e (sum, arr) = (e +. sum, e :: arr) in
  let rec subarray sq actual maximal =
    match sq with
    | e :: es ->
      if fst actual < 0.0
      then subarray sq (0.0, []) maximal
      else
        let newer = push e actual in
        if fst newer > fst maximal then subarray es newer newer else subarray es newer maximal
    | [] -> List.rev @@ snd maximal
  in
  subarray sequence (0.0, []) (0.0, [])
