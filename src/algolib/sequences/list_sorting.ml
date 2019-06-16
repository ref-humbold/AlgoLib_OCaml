(* List sorting algorithms. *)
let merge_sort cmp lst =
  let rec drop lst' n =
    if n > 0
    then
      match lst' with
      | _ :: xs -> drop xs (n - 1)
      | [] -> lst'
    else lst'
  in
  let rec merge cmp' acc lx ly =
    match (lx, ly) with
    | [], ls | ls, [] -> List.rev_append ls acc
    | x :: xs, y :: ys ->
      if cmp' x y <= 0 then merge cmp' (x :: acc) xs ly else merge cmp' (y :: acc) lx ys
  in
  let rec msort cmp' lst' n =
    match (lst', n) with
    | _, 0 -> []
    | x :: _, 1 -> [x]
    | _, _ ->
      let rev_cmp x y = -cmp' x y in
      let nd = n / 2 in
      merge rev_cmp [] (msort rev_cmp lst' nd) @@ msort rev_cmp (drop lst' nd) (n - nd)
  in
  msort cmp lst @@ List.length lst
