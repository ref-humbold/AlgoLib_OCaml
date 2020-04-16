(* Algorithms for points sorting *)
let sort_by_x lst = List.sort compare lst

let sort_by_y lst = List.sort (fun (x1, y1) (x2, y2) -> compare (y1, x1) (y2, x2)) lst

let sort_by_angle lst =
  let cmp p1 p2 =
    compare (Point2d.angle p1, Point2d.radius p1) (Point2d.angle p2, Point2d.radius p2)
  in
  List.sort cmp lst
