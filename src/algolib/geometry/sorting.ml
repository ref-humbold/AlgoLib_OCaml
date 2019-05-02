(* Points sorting algorithms. *)
type point = float * float

let angle_sort lst =
  let angle (x, y) =
    if y >= 0.0
    then (atan2 y x) *. 45.0 /. (atan 1.0)
    else (atan2 y x) *. 45.0 /. (atan 1.0) +. 360.0
  and distance (x, y) = x *. x +. y *. y in
  let cmp p1 p2 = compare (angle p1, distance p1) (angle p2, distance p2) in
  List.sort cmp lst

let lexi_sort lst = List.sort compare lst
