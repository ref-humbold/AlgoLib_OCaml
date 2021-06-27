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

let distance (Point_3d.Point3D (p1x, p1y, p1z)) (Point_3d.Point3D (p2x, p2y, p2z)) =
  sqrt
    ( ((p2x -. p1x) *. (p2x -. p1x))
      +. ((p2y -. p1y) *. (p2y -. p1y))
      +. ((p2z -. p1z) *. (p2z -. p1z)) )

let translate (Point_3d.Point3D (px, py, pz)) (Vector_3d.Vector3D (vx, vy, vz)) =
  Point_3d.pt3d (px +. vx) (py +. vy) (pz +. vz)