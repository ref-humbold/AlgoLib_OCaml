(* Algorithm for convex hull in 2D (Graham's scan). *)
open Point_2d
open Vector_2d

let find_convex_hull points =
  let find_hull points' =
    let cross_product p1 p2 p3 = area (between p2 p1) (between p2 p3) in
    let rec make_hull pts acc =
      match (pts, acc) with
      | pt :: _, h1 :: (h2 :: _ as htl) when cross_product h2 h1 pt >= 0.0 -> make_hull pts htl
      | pt :: pts', _ -> make_hull pts' (pt :: acc)
      | [], acc -> acc
    in
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
      | Some pt' -> between (pt2d_i 0 0) pt'
      | None -> failwith "unexpected"
    in
    let moving = get_moving_vector points' in
    let sorted_pts =
      Geometry_2d.sort_by_angle @@ List.map (fun pt -> Geometry_2d.translate pt ~:moving) points'
    in
    let hull_points = make_hull sorted_pts [] in
    List.rev_map (fun pt -> Geometry_2d.translate pt moving) hull_points
  in
  match points with
  | _ :: _ :: _ :: _ -> find_hull points
  | _ -> []
