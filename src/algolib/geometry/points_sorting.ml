(* Points sorting algorithms. *)
let angle_sort lst =
  let cmp p1 p2 =
    Pervasives.compare
      (Point_2d.angle p1, Point_2d.radius p1)
      (Point_2d.angle p2, Point_2d.radius p2)
  in
  List.sort cmp lst

let sort_by_x lst = List.sort compare lst

let sort_by_y lst = List.sort (fun (x1, y1) (x2, y2) -> compare (y1, x1) (y2, x2)) lst
