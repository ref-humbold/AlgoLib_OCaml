(* List sorting algorithms. *)
let merge_sort cmp lst =
  let rec drop lst' n' =
    if n' > 0
    then
      match lst' with
      | _ :: xs -> drop xs (n' - 1)
      | [] -> lst'
    else lst'
  in
  let rec merge lx ly =
    match (lx, ly) with
    | [], ls | ls, [] -> ls
    | x :: xs, y :: ys -> if cmp x y then x :: merge xs ly else y :: merge lx ys
  in
  let rec msort lst' n' =
    match (lst', n') with
    | _, 0 -> []
    | x :: _, 1 -> [x]
    | _ ->
      let nd = n' / 2 in
      merge (msort lst' nd) @@ msort (drop lst' n') (n' - nd)
  in
  msort lst @@ List.length lst
