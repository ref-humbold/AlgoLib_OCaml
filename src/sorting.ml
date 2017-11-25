(* ALGORYTMY SORTOWANIA *)
type point = float * float;;

(* Sortowanie kątowe punktów na płaszczyżnie.
   @return lista punktów posortowana względem kąta *)
let angle_sort lst =
  let angle (x, y) =
    if y >= 0.0
    then (atan2 y x) *. 45.0 /. (atan 1.0)
    else (atan2 y x) *. 45.0 /. (atan 1.0) +. 360.0
  and distance (x, y) = x *. x +. y *. y in
  let comparator p1 p2 = Pervasives.compare (angle p1, distance p1) (angle p2, distance p2) in
  List.sort comparator lst;;
