(* Algorithm for maximum_subarray. *)
let maximum_subarray sequence =
  let initial = (0.0, Seq.empty) in
  let seq_reverse sequence = Seq.fold_left (fun acc e -> Seq.cons e acc) Seq.empty sequence in
  let push e (sum, seq) = (e +. sum, Seq.cons e seq) in
  let subarray (actual, maximal) e =
    let actual' = push e actual in
    if fst actual' < 0.0
    then (initial, maximal)
    else if fst actual' > fst maximal
    then (actual', actual')
    else (actual', maximal)
  in
  let _, maximal = Seq.fold_left subarray (initial, initial) sequence in
  seq_reverse @@ snd maximal

let maximum_subarray_i sequence =
  let initial = (0, Seq.empty) in
  let seq_reverse sequence = Seq.fold_left (fun acc e -> Seq.cons e acc) Seq.empty sequence in
  let push e (sum, seq) = (e + sum, Seq.cons e seq) in
  let subarray (actual, maximal) e =
    let actual' = push e actual in
    if fst actual' < 0
    then (initial, maximal)
    else if fst actual' > fst maximal
    then (actual', actual')
    else (actual', maximal)
  in
  let _, maximal = Seq.fold_left subarray (initial, initial) sequence in
  seq_reverse @@ snd maximal
