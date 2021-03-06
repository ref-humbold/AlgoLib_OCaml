(* Algorithm for convex hull on a plane (monotone chain) *)
let find_convex_hull (points : Point2d.point2d list) =
  let cross_product (Point2d.Pt2D (p1X, p1Y)) (Point2d.Pt2D (p2X, p2Y)) (Point2d.Pt2D (p3X, p3Y)) =
    ((p1X -. p2X) *. (p3Y -. p2Y)) -. ((p3X -. p2X) *. (p1Y -. p2Y))
  in
  let rec half_hull pts acc =
    match (pts, acc) with
    | p :: ps, h1 :: h2 :: _ ->
      if cross_product h2 h1 p <= 0.0 then half_hull ps (List.tl acc) else half_hull ps (p :: acc)
    | p :: ps, _ -> half_hull ps (p :: acc)
    | [], acc -> acc
  in
  let sorted = Points_sorting.sort_2d_by_x points in
  let rsorted = List.rev sorted in
  match (sorted, rsorted) with
  | p1 :: p2 :: ps, r1 :: r2 :: rs ->
    let upper = half_hull ps [p2; p1] and lower = half_hull rs [r2; r1] in
    let rec combine lst1 lst2 acc =
      match lst2 with
      | x :: xs -> combine lst1 xs (x :: acc)
      | [] -> List.rev_append lst1 acc
    in
    combine (List.tl upper) (List.tl lower) []
  | _ -> points
