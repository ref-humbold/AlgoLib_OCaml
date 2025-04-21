(* Algorithm for pair of closest points in 2D. *)
let find_closest_points points =
  let partition points n =
    let rec partition' points' i nd =
      if i >= n then (points', nd) else partition' (List.tl points') (i + 2) (nd + 1)
    in
    partition' points 0 0
  in
  let check_belt points_y middle_x width =
    let check_points
        (Point_2d.Point2D (x1, _) as point1)
        (Point_2d.Point2D (x2, _) as point2)
        min_distance
        closest_points
      =
      if (x1 <= middle_x && middle_x <= x2) || (x2 <= middle_x && middle_x < x1)
      then
        let points_distance = Geometry_2d.distance point1 point2 in
        if points_distance < min_distance
        then ((if x1 <= x2 then Some (point1, point2) else Some (point2, point1)), points_distance)
        else (closest_points, min_distance)
      else (closest_points, min_distance)
    in
    let rec check_belt' belt_points min_distance closest_points =
      match belt_points with
      | [] -> closest_points
      | point1 :: bps1 -> check_belt'' point1 bps1 bps1 min_distance closest_points
    and check_belt''
        (Point_2d.Point2D (_, y1) as point1)
        rest1
        belt_points2
        min_distance
        closest_points
      =
      match belt_points2 with
      | [] -> check_belt' rest1 min_distance closest_points
      | (Point_2d.Point2D (_, y2) as point2) :: bps2 ->
        if y2 > y1 +. width
        then check_belt' rest1 min_distance closest_points
        else
          let closest_points', min_distance' =
            check_points point1 point2 min_distance closest_points
          in
          check_belt'' point1 rest1 bps2 min_distance' closest_points'
    in
    let belt_points =
      List.filter
        (fun (Point_2d.Point2D (x, _)) -> middle_x -. width <= x && x <= middle_x +. width)
        points_y
    in
    check_belt' belt_points width None
  in
  let search_three pt1 pt2 pt3 =
    let distance12 = Geometry_2d.distance pt1 pt2
    and distance23 = Geometry_2d.distance pt2 pt3
    and distance31 = Geometry_2d.distance pt3 pt1 in
    if distance12 <= distance23 && distance12 <= distance31
    then (pt1, pt2)
    else if distance23 <= distance12 && distance23 <= distance31
    then (pt2, pt3)
    else (pt1, pt3)
  in
  let rec search_closest points_x points_y n =
    match points_x with
    | _ when n = 0 -> raise Not_found
    | pt :: _ when n = 1 -> (pt, pt)
    | pt1 :: pt2 :: _ when n = 2 -> (pt1, pt2)
    | pt1 :: pt2 :: pt3 :: _ when n = 3 -> search_three pt1 pt2 pt3
    | _ ->
      let right_x, nd = partition points_x n in
      let middle_x, middle_y =
        match List.hd right_x with
        | Point_2d.Point2D (x, y) -> (x, y)
      in
      let left_y =
        List.filter
          (fun (Point_2d.Point2D (x, y)) -> x < middle_x || (x = middle_x && y < middle_y))
          points_y
      and right_y =
        List.filter
          (fun (Point_2d.Point2D (x, y)) -> x > middle_x || (x = middle_x && y >= middle_y))
          points_y
      in
      let closest_left = search_closest points_x left_y nd
      and closest_right = search_closest right_x right_y (n - nd) in
      let closest_points =
        if
          Geometry_2d.distance (fst closest_left) (snd closest_left)
          <= Geometry_2d.distance (fst closest_right) (snd closest_right)
        then closest_left
        else closest_right
      in
      ( match
          check_belt points_y middle_x
          @@ Geometry_2d.distance (fst closest_points) (snd closest_points)
        with
        | Some pts -> pts
        | None -> closest_points )
  in
  search_closest
    (Geometry_2d.sort_by_x @@ Geometry_2d.sort_by_y points)
    (Geometry_2d.sort_by_y points)
    (List.length points)
