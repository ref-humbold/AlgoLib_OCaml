(* Algorithm for longest common subsequence *)

let count_lcs_length sequence1 sequence2 =
  let rec iter_short short element prev_lcs next_lcs =
    match short with
    | x :: xs ->
      if x = element
      then
        let v = List.hd prev_lcs + 1 in
        iter_short xs element (List.tl prev_lcs) (v :: next_lcs)
      else
        let vs = List.tl prev_lcs in
        let n = max (List.hd vs) (List.hd next_lcs) in
        iter_short xs element vs (n :: next_lcs)
    | [] -> next_lcs
  in
  let rec iter_long long short lcs =
    match long with
    | x :: xs ->
      let next_lcs = iter_short short x (List.rev lcs) [0] in
      iter_long xs short next_lcs
    | [] -> lcs
  in
  let short_list, long_list =
    if List.compare_lengths sequence1 sequence2 <= 0
    then (sequence1, sequence2)
    else (sequence2, sequence1)
  in
  match (long_list, short_list) with
  | [], _ | _, [] -> 0
  | _ ->
    let lcs = iter_long long_list short_list @@ List.map (fun _ -> 0) short_list in
    List.hd lcs
