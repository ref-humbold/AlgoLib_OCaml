(* Algorithms for basic geometrical computations in 2D. *)
let sort_by_x lst =
  List.stable_sort (fun (Point_2d.Point2D (x1, _)) (Point_2d.Point2D (x2, _)) -> compare x1 x2) lst

let sort_by_y lst =
  List.stable_sort (fun (Point_2d.Point2D (_, y1)) (Point_2d.Point2D (_, y2)) -> compare y1 y2) lst

let sort_by_angle lst =
  let cmp p1 p2 =
    compare (Point_2d.angle_deg p1, Point_2d.radius p1) (Point_2d.angle_deg p2, Point_2d.radius p2)
  in
  List.stable_sort cmp lst

let distance (Point_2d.Point2D (p1x, p1y)) (Point_2d.Point2D (p2x, p2y)) =
  sqrt (((p2x -. p1x) *. (p2x -. p1x)) +. ((p2y -. p1y) *. (p2y -. p1y)))

let translate (Point_2d.Point2D (px, py)) (Vector_2d.Vector2D (vx, vy)) =
  Point_2d.pt2d (px +. vx) (py +. vy)

let reflect (Point_2d.Point2D (px, py)) (Point_2d.Point2D (cx, cy)) =
  Point_2d.pt2d (-.px +. cx +. cx) (-.py +. cy +. cy)
