(* Algorithms for basic geometrical computations in 3D *)

let sort_by_x lst =
  List.stable_sort
    (fun (Point_3d.Point3D (x1, _, _)) (Point_3d.Point3D (x2, _, _)) -> compare x1 x2)
    lst

let sort_by_y lst =
  List.stable_sort
    (fun (Point_3d.Point3D (_, y1, _)) (Point_3d.Point3D (_, y2, _)) -> compare y1 y2)
    lst

let sort_by_z lst =
  List.stable_sort
    (fun (Point_3d.Point3D (_, _, z1)) (Point_3d.Point3D (_, _, z2)) -> compare z1 z2)
    lst
