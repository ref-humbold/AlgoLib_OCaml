(* Algorithms for convex hull in 2D. *)
open Geometry_2d
open Point_2d
open Vector_2d

let collect_hull_ points =
  let cross_product p1 p2 p3 = area (between p2 p1) (between p2 p3) in
  let rec make_hull pts acc =
    match (pts, acc) with
    | pt :: _, h1 :: (h2 :: _ as htl) when cross_product h2 h1 pt >= 0.0 -> make_hull pts htl
    | pt :: pts', _ -> make_hull pts' (pt :: acc)
    | [], acc -> acc
  in
  make_hull points []

let find_andrew_convex_hull points =
  let find_hull points' =
    let sorted = sort_by_x points' in
    let lower_hull = collect_hull_ sorted in
    let upper_hull = collect_hull_ @@ List.rev sorted in
    let merge lst1 lst2 = List.rev_append lst1 @@ List.rev lst2 in
    merge (List.tl lower_hull) (List.tl upper_hull)
  in
  match points with
  | _ :: _ :: _ :: _ -> find_hull points
  | _ -> []

let find_graham_convex_hull points =
  let find_hull points' =
    let get_moving_vector pts =
      let minimum pts =
        List.fold_left
          (fun acc pt ->
             match (acc, pt) with
             | None, _ -> Some pt
             | Some (Point2D (ax, ay)), Point2D (px, py) ->
               if (py, px) < (ay, ax) then Some pt else acc )
          None
          pts
      in
      match minimum pts with
      | Some pt' -> between pt' Point_2d.zero
      | None -> failwith "unexpected"
    in
    let moving = get_moving_vector points' in
    let sorted = sort_by_angle @@ List.map (fun pt -> translate pt moving) points' in
    let hull_points = collect_hull_ sorted in
    List.rev_map (fun pt -> translate pt ~:moving) hull_points
  in
  match points with
  | _ :: _ :: _ :: _ -> find_hull points
  | _ -> []
