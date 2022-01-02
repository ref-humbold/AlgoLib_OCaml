(* Algorithm for longest common subsequence *)

let count_lcs_length sequence1 sequence2 =
  let rec iter_short short element prev_lcs next_lcs_rev =
    match short with
    | x :: xs ->
      ( match prev_lcs with
        | v :: vs ->
          if x = element
          then iter_short xs element vs ((v + 1) :: next_lcs_rev)
          else
            let n = max (List.hd vs) (List.hd next_lcs_rev) in
            iter_short xs element vs (n :: next_lcs_rev)
        | [] -> failwith "unexpected" )
    | [] -> next_lcs_rev
  in
  let rec iter_long long short lcs_rev =
    match long with
    | x :: xs ->
      let next_lcs_rev = iter_short short x (List.rev lcs_rev) [0] in
      iter_long xs short next_lcs_rev
    | [] -> lcs_rev
  in
  let short_list, long_list =
    if List.compare_lengths sequence1 sequence2 <= 0
    then (sequence1, sequence2)
    else (sequence2, sequence1)
  in
  match (long_list, short_list) with
  | [], _ | _, [] -> 0
  | _ ->
    let lcs =
      iter_long long_list short_list @@ List.init (List.length short_list + 1) (fun _ -> 0)
    in
    List.hd lcs

let count_lcs_length_str text1 text2 =
  let rec iter_short short len_short i element prev_lcs next_rev_lcs =
    if i < len_short
    then
      match prev_lcs with
      | v :: vs ->
        if short.[i] = element
        then iter_short short len_short (i + 1) element vs ((v + 1) :: next_rev_lcs)
        else
          let n = max (List.hd vs) (List.hd next_rev_lcs) in
          iter_short short len_short (i + 1) element vs (n :: next_rev_lcs)
      | [] -> failwith "unexpected"
    else next_rev_lcs
  in
  let rec iter_long long short len_long len_short i lcs_rev =
    if i < len_long
    then
      let next_lcs_rev = iter_short short len_short 0 long.[i] (List.rev lcs_rev) [0] in
      iter_long long short len_long len_short (i + 1) next_lcs_rev
    else lcs_rev
  in
  let short_text, long_text =
    if compare (String.length text1) (String.length text2) <= 0
    then (text1, text2)
    else (text2, text1)
  in
  let length_long = String.length long_text and length_short = String.length short_text in
  match (long_text, short_text) with
  | "", _ | _, "" -> 0
  | _ ->
    let lcs =
      iter_long long_text short_text length_long length_short 0
      @@ List.init (length_short + 1) (fun _ -> 0)
    in
    List.hd lcs
