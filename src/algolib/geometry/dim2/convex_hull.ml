(* Algorithm for convex hull in 2D (monotone chain). *)
let find_convex_hull points =
  let cross_product p1 p2 p3 = Vector_2d.area (Vector_2d.between p2 p1) (Vector_2d.between p2 p3) in
  let rec half_hull pts acc =
    match (pts, acc) with
    | p :: ps, h1 :: (h2 :: _ as htl) ->
      if cross_product h2 h1 p >= 0.0 then half_hull pts htl else half_hull ps (p :: acc)
    | p :: ps, _ -> half_hull ps (p :: acc)
    | [], acc -> acc
  in
  let sorted = Geometry_2d.sort_by_x points in
  let rsorted = List.rev sorted in
  match (sorted, rsorted) with
  | p1 :: p2 :: (_ :: _ as ps), r1 :: r2 :: (_ :: _ as rs) ->
    let lower = half_hull ps [p2; p1] and upper = half_hull rs [r2; r1] in
    let merge lst1 lst2 = List.rev_append lst1 @@ List.rev lst2 in
    merge (List.tl lower) (List.tl upper)
  | _ -> []
