(* Sorting algorithms. *)
type point = float * float

let angle_sort lst =
  let angle (x, y) =
    if y >= 0.0
    then (atan2 y x) *. 45.0 /. (atan 1.0)
    else (atan2 y x) *. 45.0 /. (atan 1.0) +. 360.0
  and distance (x, y) = x *. x +. y *. y in
  let cmp p1 p2 = Pervasives.compare (angle p1, distance p1) (angle p2, distance p2) in
  List.sort cmp lst

let merge_sort lst =
  let rec drop lst' n' =
    if n' > 0
    then match lst' with
      | _ :: xs -> drop xs @@ n' - 1
      | [] -> lst'
    else lst' in
  let rec merge lx ly =
    match lx, ly with
    | [], ls | ls, [] -> ls
    | x :: xs, y :: ys ->
      if x <= y
      then x :: (merge xs ly)
      else y :: (merge lx ys) in
  let rec msort lst' n' =
    match lst', n' with
    | _, 0 -> []
    | x :: _, 1 -> [x]
    | _ -> let nd = n' / 2 in
      merge (msort lst' nd) @@ msort (drop lst' n') @@ n' - nd in
  msort lst @@ List.length lst
