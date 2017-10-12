(* ALGORYTMY SORTOWANIA *)
type point = float * float;;

(* Sortowanie kątowe punktów na płaszczyżnie.
   @return lista punktów posortowana względem kąta *)
let angle_sort (lst : point list) =
  let angle (x, y) =
    let pi = 4.0 *. (atan 1.0) in
    if y >= 0.0
    then (atan2 y x) *. 180.0 /. pi
    else (atan2 y x) *. 180.0 /. pi +. 360.0
  and distance (x, y) = x *. x +. y *. y in
  let comparator p1 p2 =
    let a1 = angle p1 and d1 = distance p1 and a2 = angle p2 and d2 = distance p2 in
    if a1 < a2 || (a1 == a2 && d1 < d2)
    then -1
    else if a1 > a2 || (a1 == a2 && d1 > d2)
    then 1
    else 0 in
  List.sort comparator lst;;
