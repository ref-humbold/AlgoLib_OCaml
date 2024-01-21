(* Algorithms for edit distance. *)
let count_levenshtein
    ?(insertion_cost = 1.0)
    ?(deletion_cost = 1.0)
    ?(substitution_cost = 1.0)
    source
    destination
  =
  if insertion_cost < 0.0 || deletion_cost < 0.0 || substitution_cost < 0.0
  then invalid_arg "Cost cannot be negative"
  else
    let rec iter_dest dest len_dest i element prev_dist next_dist_rev =
      if i < len_dest
      then
        match prev_dist with
        | v :: vs ->
          if dest.[i] = element
          then iter_dest dest len_dest (i + 1) element vs (v :: next_dist_rev)
          else
            let n =
              min
                (min (List.hd vs +. deletion_cost) (List.hd next_dist_rev +. insertion_cost))
                (v +. substitution_cost)
            in
            iter_dest dest len_dest (i + 1) element vs (n :: next_dist_rev)
        | [] -> failwith "unexpected"
      else next_dist_rev
    in
    let rec iter_source source' dest len_source len_dest i dist_rev =
      if i < len_source
      then
        let prev_dist = List.rev dist_rev in
        let next_dist_rev =
          iter_dest dest len_dest 0 source'.[i] prev_dist [List.hd prev_dist +. deletion_cost]
        in
        iter_source source' dest len_source len_dest (i + 1) next_dist_rev
      else dist_rev
    in
    let length_source = String.length source and length_dest = String.length destination in
    let initial =
      List.of_seq @@ Seq.cons 0.0
      @@ Seq.map (fun (i, _) -> float_of_int (i + 1) *. insertion_cost)
      @@ String.to_seqi destination
    in
    let distance = iter_source source destination length_source length_dest 0 @@ List.rev initial in
    List.hd distance

let count_lcs ?(insertion_cost = 1.0) ?(deletion_cost = 1.0) source destination =
  if insertion_cost < 0.0 || deletion_cost < 0.0
  then invalid_arg "Cost cannot be negative"
  else
    let rec iter_dest dest len_dest i element prev_dist next_dist_rev =
      if i < len_dest
      then
        match prev_dist with
        | v :: vs ->
          if dest.[i] = element
          then iter_dest dest len_dest (i + 1) element vs (v :: next_dist_rev)
          else
            let n = min (List.hd vs +. deletion_cost) (List.hd next_dist_rev +. insertion_cost) in
            iter_dest dest len_dest (i + 1) element vs (n :: next_dist_rev)
        | [] -> failwith "unexpected"
      else next_dist_rev
    in
    let rec iter_source source' dest len_source len_dest i dist_rev =
      if i < len_source
      then
        let prev_dist = List.rev dist_rev in
        let next_dist_rev =
          iter_dest dest len_dest 0 source'.[i] prev_dist [List.hd prev_dist +. deletion_cost]
        in
        iter_source source' dest len_source len_dest (i + 1) next_dist_rev
      else dist_rev
    in
    let length_source = String.length source and length_dest = String.length destination in
    let initial =
      List.of_seq @@ Seq.cons 0.0
      @@ Seq.map (fun (i, _) -> float_of_int (i + 1) *. insertion_cost)
      @@ String.to_seqi destination
    in
    let distance = iter_source source destination length_source length_dest 0 @@ List.rev initial in
    List.hd distance

let count_hamming ?(substitution_cost = 1.0) source destination =
  if substitution_cost < 0.0
  then invalid_arg "Cost cannot be negative"
  else
    let source' = List.of_seq @@ String.to_seq source
    and destination' = List.of_seq @@ String.to_seq destination in
    try
      List.fold_left2
        (fun acc c1 c2 -> if c1 = c2 then acc else acc +. substitution_cost)
        0.0
        source'
        destination'
    with
    | Invalid_argument _ -> invalid_arg "Texts must have equal length"
