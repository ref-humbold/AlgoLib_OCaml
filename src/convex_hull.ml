(* OTOCZKA WYPUKŁA PUNKTÓW NA PŁASZCZYŹNIE *)
type point = float * float

(* Wyznaczanie otoczki wypukłej.
   @return lista punktów otoczki *)
let find_convex_hull (points : point list) =
  let cross_product (p1X, p1Y) (p2X, p2Y) (p3X, p3Y) =
    (p1X - p2X) * (p3Y - p2Y) - (p3X - p2X) * (p1Y - p2Y) in
  let rec half_hull pts acc =
    match (pts, acc) with
    | (p::ps, h1::h2::_) ->
      if cross_product h2 h1 p <= 0
      then half_hull ps @@ List.tl acc
      else half_hull ps @@ p::acc
    | (p::ps, _) -> half_hull ps @@ p::acc
    | ([], acc) -> acc in
  let sorted = List.sort (fun p1 p2 -> if p1 < p2 then -1 else if p1 > p2 then 1 else 0) points
  and rsorted = List.sort (fun p1 p2 -> if p1 < p2 then 1 else if p1 > p2 then -1 else 0) points in
  match (sorted, rsorted) with
  | (p1::p2::ps, r1::r2::rs) ->
    let upper = half_hull ps [p2, p1] and lower = half_hull rs [r2, r1] in
    let rec combine lst1 lst2 acc =
      match lst2 with
      | x::xs -> combine lst1 xs @@ x::acc
      | [] -> List.rev_append lst1 acc in
    combine (List.tl upper) (List.tl lower) []
  | _ -> points
