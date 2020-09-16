(* Algorithms for points sorting *)

let sort_2d_by_x lst =
  List.stable_sort (fun (Point2d.Pt2D (x1, _)) (Point2d.Pt2D (x2, _)) -> compare x1 x2) lst

let sort_3d_by_x lst =
  List.stable_sort (fun (Point3d.Pt3D (x1, _, _)) (Point3d.Pt3D (x2, _, _)) -> compare x1 x2) lst

let sort_2d_by_y lst =
  List.stable_sort (fun (Point2d.Pt2D (_, y1)) (Point2d.Pt2D (_, y2)) -> compare y1 y2) lst

let sort_3d_by_y lst =
  List.stable_sort (fun (Point3d.Pt3D (_, y1, _)) (Point3d.Pt3D (_, y2, _)) -> compare y1 y2) lst

let sort_3d_by_z lst =
  List.stable_sort (fun (Point3d.Pt3D (_, _, z1)) (Point3d.Pt3D (_, _, z2)) -> compare z1 z2) lst

let sort_2d_by_angle lst =
  let cmp p1 p2 =
    compare (Point2d.angle_deg p1, Point2d.radius p1) (Point2d.angle_deg p2, Point2d.radius p2)
  in
  List.stable_sort cmp lst
